{-# LANGUAGE LambdaCase, TupleSections, FlexibleInstances, RankNTypes #-}
-- {-# LANGUAGE Strict #-} -- Rend le langage strict pour de meilleures performances
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module VM where

import Data.Char ( ord, chr )
import Data.Foldable ( foldrM )
import Data.IORef ( IORef )

import Utils
import Rib

inputStr :: String
inputStr = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" -- RVM code that prints HELLO!

emptySymbolsCount :: Int
symbolTableStr, instructionsStr :: String
((symbolTableStr, emptySymbolsCount), instructionsStr) =
  let (start, end) = span (/= ';') inputStr
  in (readInt start 0, drop 1 end)

-- Reading input

-- Positive numbers outside [0,45] are encoded using a variable length encoding.
-- If the number is encoded in k characters (codes), the first k-1 are in
-- [46, 91] and the k^th code is in [0,45] to mark the end of the number.
-- The codes encode the number in base-46, and are interpreted modulo 46.
readInt :: String -> Int -> (String, Int)
readInt [] n = ([], n)
readInt (x:xs) n =
  let v = ord x - 35
      c = if v < 0 then 57 else v -- error "Bad code, less than 35" else v -- python returns 57 instead of error
      n' = n * 46
  in if c < 46 then (xs, n' + v) else readInt xs (n' + v - 46)

-- VM environment

data State = State
  { stackRef       :: IORef Rib
  , symbolTableRef :: IORef Rib
  , falseRef       :: IORef Rib
  , trueRef        :: IORef Rib
  , nilRef         :: IORef Rib
  }

type Prim = ReaderIO State ()

push :: RibRef a => a -> ReaderIO State ()
push v = do
  -- Get stack reference
  stackPtr <- fmap stackRef get
  -- Save top of stack
  stack <- readRef stackPtr
  -- Cons v to top of stack
  newStack <- cons v stack
  -- Update stack reference to point to new top of stack
  writeRef stackPtr newStack

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

prim1 :: (Rib -> ReaderIO State Rib) -> Prim
prim1 f = do
  v1 <- pop
  x <- f v1
  push x

prim2 :: (Rib -> Rib -> ReaderIO State Rib) -> Prim
prim2 f = do
  v1 <- pop
  v2 <- pop
  x <- f v1 v2
  push x

-- Note: Contrairement à prim1 et prim2, f prend des IORef car la seule
-- utilisation de prim3 n'a pas besoin de lire les références.
prim3 :: (IORef Rib -> IORef Rib -> IORef Rib -> Rib) -> Prim
prim3 f = do
  v1 <- popFast
  v2 <- popFast
  v3 <- popFast
  let x = f v1 v2 v3
  push x

primitives :: [Prim]
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
  , prim1 read1                                                                       -- field0 -- 6
  , prim1 read2                                                                       -- field1 -- 7
  , prim1 read3                                                                       -- field2 -- 8
  , prim2 (\r v -> write1 r v >> pure v)                                              -- field0-set! -- 9
  , prim2 (\r v -> write2 r v >> pure v)                                              -- field1-set! -- 10
  , prim2 (\r v -> write3 r v >> pure v)                                              -- field2-set! -- 11
  , prim2 (\(RibInt r1) (RibInt r2) -> pure $ if r1 < r2 then ribTrue else ribFalse)  -- <
  , prim2 (\(RibInt r1) (RibInt r2) -> pure $ RibInt (r1 + r2))                       -- add
  , prim2 (\(RibInt r1) (RibInt r2) -> pure $ RibInt (r1 - r2))                       -- sub
  , prim2 (\(RibInt r1) (RibInt r2) -> pure $ RibInt (r1 * r2))                       -- mult
  , prim2 (\(RibInt r1) (RibInt r2) -> pure $ RibInt (div r1 r2))                     -- quotient
  , liftIO getChar >>= push . RibInt . ord                                            -- getChar
  , prim1 (\r@(RibInt v) -> liftIO (putChar (chr v)) >> pure r)                       -- putChar
  , prim1 (\c -> push c >> pure c)  -- Duplicate top element. Not a regular RVM primitive, but useful in Repl.hs
  ]

-- Initializing symbol table

-- Définition alternative de initialSymbolTable plus idiomatique.
initialSymbolTable :: String -> Int -> IO Rib
initialSymbolTable symTblStr emptySymCount = do
  let symbolStrings = splitOnCommas symTblStr
      -- On ajoute les symboles sans string
      symbolStringsWithEmpty = replicate emptySymCount "" <> symbolStrings
  -- Pour chaque symbole, on encode son string en Rib
  symbolStringRibs <- mapM toRibSymbol symbolStringsWithEmpty
  -- On encode la liste dans l'ordre inverse
  toRibList (reverse symbolStringRibs)
  where
    -- Brise le string sur les virgules
    splitOnCommas xs =
      case span (/= ',') xs of
        (sym, "") -> [reverse sym]
        (sym, rest) -> reverse sym : splitOnCommas (drop 1 rest)

-- Decoding RVM instructions

symbolRef :: Int -> ReaderIO State Rib
symbolRef n = do
  symbolTable <- symbolTableRef <$> get
  read1 =<< listTail n =<< readRef symbolTable

listTail :: MonadIO m => Int -> Rib -> m Rib
listTail = \case 0 -> pure; n -> \r -> read2 r >>= listTail (n-1)

decodeInstructions :: String -> ReaderIO State Rib
decodeInstructions instrStr = do
  stackPtr <- fmap stackRef get
  let
      go :: String -> ReaderIO State Rib
      go [] = pop
      go (x:rest) = do
        -- First code tells us the operand
        let c = ord x - 35
            code = if c < 0 then 57 else c
            (n, d, op) = go2 code 0
        if c > 90
        then do
          tos <- pop
          stack <- readRef stackPtr
          stack1 <- read1 stack
          write1 stack =<< mkInstr (op - 1) tos stack1
          go rest
        else do
          op <- if op == 0
                  then push (RibInt 0) >> pure (op + 1)
                  else pure op

          (rest', n) <- if n == d
                          then do
                          -- get_int(0)
                          -- let (rest', i) = readInt rest 0
                          -- pure (rest', RibInt i)
                          pure (RibInt <$> readInt rest 0)
                        else if n >= d
                        then do
                          -- symbol_ref(get_int(n - d - 1))
                          let (rest', i) = readInt rest (n - d - 1)
                          (rest',) <$> symbolRef i
                        else if op < 3
                          -- symbol_ref(n)
                        then (rest,) <$> symbolRef n
                        else pure (rest, RibInt n)

          n <- if 4 < op
            then do
              tos <- pop
              b <- mkInstr n (RibInt 0) tos
              mkInstr b (RibInt 0) (RibInt 1)
            else pure n

          readRef stackPtr >>= \case
            RibInt i -> read1 n >>= read3 -- End: pc = n[0][2]
            RibObj v1 _ _ -> do
              readRef v1 >>=
                mkInstr (min 4 op - 1) n >>=
                  writeRef v1
              go rest'

      -- Finds the op code from the encoded instruction
      go2 n op =
        let d = [20,30,0,10,11,4] !! op
        in if n <= 2+d then (n, d, op) else go2 (n-d-3) (op+1)

  go instrStr

-- FIXME: We lose the elements of the symbol table.
-- It would be nice if they were kept and available to display when debugging.
-- For now, the symbol table reference is restored after the calls to setGlobal.
-- Returns a reference to the global object so we can use IORef equality.
setGlobal :: IORef Rib -> String -> Rib -> IO (IORef Rib)
setGlobal symbolTablePtr gloName val = do
  symbolTable <- readRef symbolTablePtr
  -- symtbl[0][0]=val
  symbolTableFst@(RibObj r1 _ _) <- read1 symbolTable
  write1 symbolTableFst val
  -- Give name to global variable.
  -- Equivalent to symtbl[0][1]=val
  -- Note: This is not in the python implementation
  write2 symbolTableFst =<< toRibString gloName
  -- symtbl=symtbl[1]
  writeRef symbolTablePtr =<< read2 symbolTable
  pure r1

