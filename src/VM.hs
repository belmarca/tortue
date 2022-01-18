{-# LANGUAGE LambdaCase, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DerivingVia #-}
module VM where

import Data.Char ( ord, chr )
import GHC.Arr ( Array, listArray, unsafeAt )
import Data.Word ( Word8 )
import Data.IORef ( IORef )
import GHC.IO (unsafePerformIO)

import Utils
import Data.Foldable ( foldrM )

-- TODO: Unbox me
input :: Array Int Word8
input = listArray (0, length inputStr - 1) $ fmap (toEnum . ord) inputStr

inputStr :: String
inputStr = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" -- RVM code that prints HELLO!

emptySymbolsCount :: Int
symbolTableStr, instructionsStr :: String
((symbolTableStr, emptySymbolsCount), instructionsStr) =
  let (start, end) = span (/= ';') inputStr
  in (readInt start 0, drop 1 end)

-- Reading input

-- Warning: Unlike Python's get_byte, this doesn't increment pc
getByte :: Int -> Word8
getByte = unsafeAt input

getCode :: Int -> Word8
getCode pos =
  let x = getByte pos - 35
  in if x < 0 then 57 else x

-- Positive numbers outside [0,45] are encoded using a variable length encoding.
-- If the number is encoded in k characters (codes), the first k-1 are in
-- [46, 91] and the k^th code is in [0,45] to mark the end of the number.
-- The codes encode the number in base-46, and are interpreted modulo 46.
getInt :: Int -> Int -> (Int, Int)
getInt n pos =
  let x = fromEnum $ getCode pos
      n' = n * 46
  in if x < 46 then (n' + x, pos + 1) else getInt (n' + x - 46) (pos + 1)

readInt :: String -> Int -> (String, Int)
readInt [] n = ([], n)
readInt (x:xs) n =
  let v = ord x - 35
      c = if v < 0 then error "Bad code, less than 35" else v -- python returns 57 instead of error
      n' = n * 46
  in if c < 46 then (xs, n' + v) else readInt xs (n' + v - 46)

-- Rib Objects

data Rib = RibInt Int | RibObj (IORef Rib) (IORef Rib) (IORef Rib)

-- Typeclass permettant d'utilisation de mkObj sans devoir à toujours créer des
-- IORef manuellement.
-- toRibRef permet la conversion d'une valeur de type a en IORef Rib par une action IO.
class RibRef a where
  toRibRef :: a -> IO (IORef Rib)

-- Rien à faire, on a déjà un IORef Rib
instance RibRef (IORef Rib) where
  toRibRef = pure

-- On crée une référence pour le Rib
instance RibRef Rib where
  toRibRef = newRef

-- On wrap le Int dans RibInt et on crée une référence
instance RibRef Int where
  toRibRef = newRef . RibInt

-- On convertit le char en RibInt et on crée une référence
instance RibRef Char where
  toRibRef = newRef . RibInt . ord

mkObj :: (RibRef a, RibRef b, RibRef c, MonadIO m) => a -> b -> c -> m Rib
mkObj n r2 r3 = RibObj <$> liftIO (toRibRef r2) <*> liftIO (toRibRef r3) <*> liftIO (toRibRef n)

read1, read2, read3 :: MonadIO m => Rib -> m Rib
read1 (RibObj v _ _) = readRef v
read2 (RibObj _ v _) = readRef v
read3 (RibObj _ _ v) = readRef v

-- write1, write2, write3 :: MonadIO m => IORef Rib -> Rib -> m ()

mkPair :: (RibRef a, RibRef b, MonadIO m) => a -> b -> m Rib
mkPair = mkObj (0 :: Int)

cons :: (RibRef a, RibRef b, MonadIO m) => a -> b -> m Rib
cons = mkPair

mkProc :: (RibRef a, RibRef b, MonadIO m) => a -> b -> m Rib
mkProc = mkObj (1 :: Int)

mkSymb :: (RibRef a, RibRef b, MonadIO m) => a -> b -> m Rib
mkSymb = mkObj (2 :: Int)

mkStr :: (RibRef a, RibRef b, MonadIO m) => a -> b -> m Rib
mkStr  = mkObj (3 :: Int)

mkVect :: (RibRef a, RibRef b, MonadIO m) => a -> b -> m Rib
mkVect = mkObj (4 :: Int)

mkSVal :: MonadIO m => m Rib
mkSVal = mkObj (5 :: Int) (RibInt 0) (RibInt 0) -- Don't care about zeroes

{-# NOINLINE ribFalse #-}
ribFalse :: Rib
ribFalse = unsafePerformIO mkSVal

{-# NOINLINE ribTrue #-}
ribTrue :: Rib
ribTrue = unsafePerformIO mkSVal

ribNil :: Rib
ribNil = ribTrue

-- Fonctions de conversion Haskell -> Rib

toRibList :: (RibRef a, MonadIO m) => [a] -> m Rib
toRibList = foldrM cons ribNil

toRibString :: MonadIO m => String -> m Rib
toRibString chars = do
  ribLst <- toRibList $ fmap fromEnum chars
  mkStr ribLst (length chars)

toRibSymbol :: MonadIO m => String -> m Rib
toRibSymbol chars = mkSymb (RibInt 0) =<< toRibString chars

-- VM environment

data State = State
  { stackRef       :: IORef Rib
  , symbolTableRef :: IORef Rib
  }

type Fun = ReaderIO State ()

push :: Rib -> ReaderIO State ()
push v = do
  -- Get stack reference
  stackPtr <- fmap stackRef get
  -- Cons v to stack
  newStack <- cons v stackPtr
  -- Update stack reference to point to new stack
  liftIO $ writeRef stackPtr newStack

pop :: ReaderIO State Rib
pop = do
  ref <- popFast
  readRef ref

popFast :: ReaderIO State (IORef Rib)
popFast = do
  -- Get stack reference
  stackPtr <- fmap stackRef get
  stack <- readRef stackPtr
  case stack of
    RibInt _ -> error "Empty stack"
    RibObj topRef restRef _ -> do
      -- Update stack reference to point to rest of stack
      rest <- readRef restRef
      writeRef stackPtr rest
      -- Return value
      pure topRef

-- Primitives

prim1 :: (Rib -> ReaderIO State Rib) -> Fun
prim1 f = do
  v1 <- pop
  x <- f v1
  push x

prim2 :: (Rib -> Rib -> ReaderIO State Rib) -> Fun
prim2 f = do
  v1 <- pop
  v2 <- pop
  x <- f v1 v2
  push x

-- Note: Contrairement à prim1 et prim2, f prend des IORef car la seule
-- utilisation de prim3 n'a pas besoin de lire les références.
prim3 :: (IORef Rib -> IORef Rib -> IORef Rib -> Rib) -> Fun
prim3 f = do
  v1 <- popFast
  v2 <- popFast
  v3 <- popFast
  let x = f v1 v2 v3
  push x

primitives :: [Fun]
primitives =
  [ prim3 RibObj                                                                      -- rib object constructor
  , prim1 pure                                                                        -- id
  , void pop                                                                          -- take 2 TOS, keep first
  , prim2 (const pure)                                                                -- take 2 TOS, keep second
  , do
      RibObj v1 _ _ <- pop
      stackPtr <- stackRef <$> get
      mkProc v1 stackPtr >>= push                                                     -- close
  , prim1 (pure . (\case RibInt _ -> ribFalse; _ -> ribTrue))                         -- rib?
  , prim1 read1                                                                       -- field0
  , prim1 read2                                                                       -- field1
  , prim1 read3                                                                       -- field2
  , prim2 (\(RibObj r _ _) v -> writeRef r v >> pure v)                               -- field0-set!
  , prim2 (\(RibObj _ r _) v -> writeRef r v >> pure v)                               -- field1-set!
  , prim2 (\(RibObj _ _ r) v -> writeRef r v >> pure v)                               -- field2-set!
  , prim2 (\(RibInt r1) (RibInt r2) -> pure $ if r1 < r2 then ribTrue else ribFalse)  -- <
  , prim2 (\(RibInt r1) (RibInt r2) -> pure $ RibInt (r1 + r2))                       -- add
  , prim2 (\(RibInt r1) (RibInt r2) -> pure $ RibInt (r1 - r2))                       -- sub
  , prim2 (\(RibInt r1) (RibInt r2) -> pure $ RibInt (r1 * r2))                       -- mult
  , prim2 (\(RibInt r1) (RibInt r2) -> pure $ RibInt (div r1 r2))                     -- quotient
  , liftIO getChar >>= push . RibInt . ord                                            -- getChar
  , prim1 (\r@(RibInt v) -> liftIO (putChar (chr v)) >> pure r)                       -- putChar
  ]

-- Initializing symbol table

-- Int returned corresponds to the cursor position in the input
initialSymbolTable :: IO (Int, Rib)
initialSymbolTable = do
  -- Symbols with no string representations
  let go1 n s = do
        if n >= 0
          then do
            -- Empty string
            str <- mkStr ribNil (RibInt 0)
            -- Symbol with 0 as value
            sym <- mkSymb (RibInt 0) str
            -- Append the symbol to the symbol table
            s' <- cons sym s
            go1 (n-1) s'
          else pure s

      -- Symbols with string representations
      go2 :: Int -> Int -> Rib -> Rib -> IO (Int, Rib)
      go2 pos n acc s = do
        let c = getByte pos
            appendAcc = do
              -- String from acc
              str <- mkStr acc (RibInt n)
              -- Symbol with 0 as value
              sym <- mkSymb (RibInt 0) str
              -- Append the symbol to the symbol table
              cons sym s
        if c == 44
          -- 44 = ',' end of element
          then appendAcc >>= go2 (pos + 1) 0 ribNil
          -- 59 = ';', end of symbol table
          else if c == 59 then (pos + 1,) <$> appendAcc
          else do
            -- Append character to acc
            acc' <- cons (RibInt $ fromEnum c) acc
            go2 (pos + 1) (n+1) acc' s

  let (i, i') = getInt 0 0
  go1 i ribNil >>= go2 i' 0 ribNil

-- Définition alternative de initialSymbolTable plus idiomatique.
initialSymbolTable' :: IO Rib
initialSymbolTable' = do
  let symbolStrings = splitOnCommas symbolTableStr
      -- On ajoute les symboles sans string
      symbolStringsWithEmpty = replicate emptySymbolsCount "" <> symbolStrings
  -- Pour chaque symbole, on encode son string en Rib
  symbolStringRibs <- mapM toRibSymbol symbolStringsWithEmpty
  -- On encode la liste
  toRibList symbolStringRibs
  where
    -- Brise le string sur les virgules
    splitOnCommas [] = []
    splitOnCommas xs = let (sym, rest) = span (/= ',') xs in sym : splitOnCommas (drop 1 rest)

{-

-- Decoding RVM instructions

symbolRef :: Int -> Rib -> IO Rib
symbolRef n s = do
  RibObj v1 _ _ <- listTail n s
  readIORef v1

listTail :: Int -> Rib -> IO Rib
listTail = \case 0 -> pure; n -> \r -> read2 r >>= listTail (n-1)

decodeInstructions :: State -> Int -> IO (State, Rib)
decodeInstructions st pos = do
  let go st pos = do
        -- First code tells us the
        let c = getCode pos
            (n, d, op) = go2 n op
        (n, d, op, st) <-
          if c > 90
          then do
            (st, RibInt n) <- pop st
            pure (n, d, op, st)
          else do
            (st, op) <- if op == 0 then push st (RibInt 0) else pure (st)
            (n, pos') <- if n == d
                          then let (i,pos') = getInt 0 pos
                            in pure (RibInt i, pos')
                          else if n>=d
                            then do
                                let (i, pos') = getInt (n-d-1) pos
                                n <- symbolRef i (symbolTable st)
                                pure (n, pos')
                          else if op < 3
                            then do
                              n <- symbolRef n (symbolTable st)
                              pure (n, pos)
                          else pure (RibInt n, pos)
            n <- if 4 < op
              then do
                (st, a) <- pop st
                b <- mkObj' n (RibInt 0) a
                mkObj 1 b (RibInt 0)
              else pure n

            case stack st of
              RibInt i -> undefined -- stop
              RibObj v1 _ _ ->
                readIORef v1 >>=
                  mkObj' (RibInt $ min 4 op - 1) n >>=
                    writeRef v1

            pure (n, d, op, st)


        undefined

        -- if n < op then
        --   let n =

        -- let RibObj v1 _ _ = stack st
        -- v <- readIORef v1
        -- writeRef v1 =<< mkObj' (RibInt $ op - 1) (RibInt n) v
        -- go st (pos + 1 + getIntOffset)

      -- Finds the op code from the encoded instruction
      go2 n op =
        let d = [20,30,0,10,11,4] !! op
        in if n <= 2+d then (n, d, op) else go2 (n-d-3) (op+1)

  undefined

-}
