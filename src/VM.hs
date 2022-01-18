{-# LANGUAGE LambdaCase, TupleSections, FlexibleInstances #-}
{-# LANGUAGE Strict #-} -- Rend le langage strict pour de meilleures performances
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module VM where

import Data.Char ( ord, chr )
import Data.Foldable ( foldrM )
import Data.IORef ( IORef )
import Data.Word ( Word8 )
import GHC.Arr ( Array, listArray, unsafeAt )
import GHC.IO (unsafePerformIO)

import Utils
import Rib
import GHC.Stack (HasCallStack)

-- TODO: Unbox me
input :: Array Int Word8
input = listArray (0, length inputStr - 1) $ fmap (toEnum . ord) inputStr

-- inputStr = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" -- RVM code that prints HELLO!
inputStr :: String
inputStr = "Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,"
        <> "enifed,!tes-rotcev,?rotcev,=,cc/"
        <> "llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,"
        <> "lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,enilwen,ton,"
        <> "lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,rotcev>-tsil,+,"
        <> "etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,"
        <> "htgnel,,,,,rddac,rdac,,-,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8K!"
        <> "K8K@Z%@YGZ#^'i$~YM^YC@PvCvR3y]#7#YS*^z!S*9Bi&:EiS/ai&kkz!S/"
        <> ":kw'k]@'_*Z@aC_G^~F^{!>'^8>YHlbC`^'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~"
        <> "F_|!S+#`kn5^~i$#`kn5^~i$#`kn5^~i$#`kn5^~RL^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_"
        <> "k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/"
        <> "NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#"
        <> "ZCex>#d~TbZBi&:EiS/"
        <> "NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?"
        <> "x=^G_~F_#bUk``m~YL_|!93_@J^{]%3uy]?'i$9?C_@J^G^~F^z]I'i$'i$9IC^@YGG^~F^@"
        <> "JvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YJ^8EZ)^~YL^3vL@ZIC^@YGG^@JvK~F^89vLvK~T^"
        <> "89vS;vF~?i%^89vS-vF~Z$^z!G8E^3vE@Z?i%YD^@JvE~YJ^z]O9O8@~?u^'^~Ik^Dy!@8@@D'"
        <> "^9O~?vR0^~I_vC'iS0~YM^YFy!?*V^@D'i&~OOIvD`*V^@D'i&~OO^~^?vL_*V^@D'i&~O^~^?"
        <> "vK^YFy]M*ZM^YC'i&@D~?vL^Wy!C9*`'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC'i%@D~?"
        <> "vS;^'i$@D~?vS-^YF@D~?vF^9M@D~?vK^'^~Ik^Wy!F'^!S-^Dy]H'^!S-iS.'^~?iS0^!S-^"
        <> "z!-9H^9HYS#~?iS.^'^~?iS0^iS-y!S-iS.!M(iS0^z]27%Z>'_@YS&Lc^@YS'Hc^BBZ>i$"
        <> "zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-8P`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^"
        <> "z]79Nb`H^|];8P`H^{]<i+!Di1!B#nS_^z!JQn]F'_'i$'i$9FKKvR%`YNbuC_~IvR/"
        <> "^~I_vR$G^~F^{]G9Fk^'i$~T^z!S%'i$4_k~^ZG^9GC^~?vPG^'i$~T^YD^z]E'^9E_`~"
        <> "IakAb^YHKYNu``vR%Z&u^{!S(8BZEi&^8BAZEi&K`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P'^!S,"
        <> "AiS,^YS$^9PBa_'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!LQm]J'`9JAca`Kl^~I_k|]"
        <> "L9Ji&`^{]A'^9AKl`C^~I`k{]N9'aZA`^|!P0ZA`^{!<'k8HSC_l~F^z!=(i&^z!O87B^z!"
        <> "76B^z]/+B^z!61B^z]9iS)]'iS'!,i+!0i1!*#k`^{!/"
        <> "Qk!A'i$'i$'i$'i$8ALaL_~YABaB_~YAHaH_~R`~R_'^~^?`^{]$(i$^z!:9>'i$(bL^~R^zz!"
        <> "S.Kmk!S0Klk!':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!5:lks!S':lkt!S):lku!"
        <> "S&:lkv.!(:lkv/!2:lkv0!H:lkv1!4:lkv2!N:lkv3]&:lkv4!S#:lkv5!3:lkv6y"

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

-- VM environment

data State = State
  { stackRef       :: IORef Rib
  , symbolTableRef :: IORef Rib
  }

type Fun = ReaderIO State ()

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
    splitOnCommas xs = let (sym, rest) = span (/= ',') xs in reverse sym : splitOnCommas (drop 1 rest)

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
