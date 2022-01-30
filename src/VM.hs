{-# LANGUAGE LambdaCase, TupleSections, FlexibleInstances, RankNTypes #-}
-- {-# LANGUAGE Strict #-} -- Rend le langage strict pour de meilleures performances
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module VM where

import Control.Monad
import Data.Char ( ord, chr )
import Data.Foldable ( foldrM )
import Data.IORef ( IORef )

import Utils
import Rib
import Env

import Debug.Trace

inputStr :: String
-- inputStr = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" -- RVM code that prints HELLO!
inputStr = "Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,enifed,!tes-rotcev,?rotcev,=,cc/llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,enilwen,ton,lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,rotcev>-tsil,+,etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,htgnel,,,,,rddac,rdac,,-,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8K!K8K@Z%@YGZ#^'i$~YM^YC@PvCvR3y]#7#YS*^z!S*9Bi&:EiS/ai&kkz!S/:kw'k]@'_*Z@aC_G^~F^{!>'^8>YHlbC`^'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~F_|!S+#`kn5^~i$#`kn5^~i$#`kn5^~i$#`kn5^~RL^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?x=^G_~F_#bUk``m~YL_|!93_@J^{]%3uy]?'i$9?C_@J^G^~F^z]I'i$'i$9IC^@YGG^~F^@JvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YJ^8EZ)^~YL^3vL@ZIC^@YGG^@JvK~F^89vLvK~T^89vS;vF~?i%^89vS-vF~Z$^z!G8E^3vE@Z?i%YD^@JvE~YJ^z]O9O8@~?u^'^~Ik^Dy!@8@@D'^9O~?vR0^~I_vC'iS0~YM^YFy!?*V^@D'i&~OOIvD`*V^@D'i&~OO^~^?vL_*V^@D'i&~O^~^?vK^YFy]M*ZM^YC'i&@D~?vL^Wy!C9*`'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC'i%@D~?vS;^'i$@D~?vS-^YF@D~?vF^9M@D~?vK^'^~Ik^Wy!F'^!S-^Dy]H'^!S-iS.'^~?iS0^!S-^z!-9H^9HYS#~?iS.^'^~?iS0^iS-y!S-iS.!M(iS0^z]27%Z>'_@YS&Lc^@YS'Hc^BBZ>i$zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-8P`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^z]79Nb`H^|];8P`H^{]<i+!Di1!B#nS_^z!JQn]F'_'i$'i$9FKKvR%`YNbuC_~IvR/^~I_vR$G^~F^{]G9Fk^'i$~T^z!S%'i$4_k~^ZG^9GC^~?vPG^'i$~T^YD^z]E'^9E_`~IakAb^YHKYNu``vR%Z&u^{!S(8BZEi&^8BAZEi&K`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P'^!S,AiS,^YS$^9PBa_'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!LQm]J'`9JAca`Kl^~I_k|]L9Ji&`^{]A'^9AKl`C^~I`k{]N9'aZA`^|!P0ZA`^{!<'k8HSC_l~F^z!=(i&^z!O87B^z!76B^z]/+B^z!61B^z]9iS)]'iS'!,i+!0i1!*#k`^{!/Qk!A'i$'i$'i$'i$8ALaL_~YABaB_~YAHaH_~R`~R_'^~^?`^{]$(i$^z!:9>'i$(bL^~R^zz!S.Kmk!S0Klk!':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!5:lks!S':lkt!S):lku!S&:lkv.!(:lkv/!2:lkv0!H:lkv1!4:lkv2!N:lkv3]&:lkv4!S#:lkv5!3:lkv6y"

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

type Prim = ReaderIO State ()

push :: RibRef a => a -> ReaderIO State ()
push v = get >>= flip push' v . stackRef

push' :: RibRef a => IORef Rib -> a -> ReaderIO State ()
push' stackPtr v = do
  -- Save top of stack
  stack <- readRef stackPtr
  -- Cons v to top of stack
  newStack <- cons v stack
  -- Update stack reference to point to new top of stack
  writeRef stackPtr newStack

pop :: ReaderIO State Rib
pop = popFast >>= readRef

popFast :: ReaderIO State (IORef Rib)
popFast = get >>= popFast' . stackRef

pop' :: MonadIO m => IORef Rib -> m Rib
pop' stackPtr = popFast' stackPtr >>= readRef

popFast' :: MonadIO m => IORef Rib -> m (IORef Rib)
popFast' stackPtr = do
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
prim2 f = prim2' (\r1 r2 -> join (f <$> readRef r1 <*> readRef r2))

prim2' :: (IORef Rib -> IORef Rib -> ReaderIO State Rib) -> Prim
prim2' f = do
  v2 <- popFast
  v1 <- popFast
  x <- f v1 v2
  push x

-- Note: Contrairement à prim1 et prim2, f prend des IORef car la seule
-- utilisation de prim3 n'a pas besoin de lire les références.
prim3 :: (IORef Rib -> IORef Rib -> IORef Rib -> Rib) -> Prim
prim3 f = do
  v3 <- popFast
  v2 <- popFast
  v1 <- popFast
  let x = f v1 v2 v3
  push x

primitives :: [Prim]
primitives = zipWith decorator [0..]
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
  , prim2' $ \r1 r2 ->
      readRef r1 >>= \case
        RibObj {} -> pure $ toBool (r1 == r2)
        RibInt x -> readRef r2 >>= \case
          RibObj {} -> pure ribFalse
          RibInt y -> pure $ toBool (x == y)                                          -- eqv?
  , prim2 $ \(RibInt r1) (RibInt r2) -> pure $ toBool (r1 < r2)                       -- <
  , prim2 $ \(RibInt r1) (RibInt r2) -> pure $ RibInt (r1 + r2)                       -- add
  , prim2 $ \(RibInt r1) (RibInt r2) -> pure $ RibInt (r1 - r2)                       -- sub
  , prim2 $ \(RibInt r1) (RibInt r2) -> pure $ RibInt (r1 * r2)                       -- mult
  , prim2 $ \(RibInt r1) (RibInt r2) -> pure $ RibInt (div r1 r2)                     -- quotient
  , liftIO getChar >>= push . RibInt . ord                                            -- getChar
  , prim1 (\r@(RibInt v) -> liftIO (putChar (chr v)) >> pure r)                       -- putChar
  -- , prim1 (\c -> push c >> pure c)  -- Duplicate top element. Not a regular RVM primitive, but useful in Repl.hs
  ]
  where
    toBool b = if b then ribTrue else ribFalse

    -- TODO: Minimize me
    decorator :: Int -> Prim -> Prim
    decorator primId prim = do
      -- traceShowM $ "Calling primitive " <> show primId
      prim

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
listTail = \case 0 -> pure; n -> read2 >=> listTail (n-1)

decodeInstructions :: String -> ReaderIO State Rib
decodeInstructions instrStr = do
  stackPtr <- fmap stackRef get
  let
      go :: String -> ReaderIO State Rib
      go [] = pop -- TODO: On devrait pas retourner read3 =<< read1 =<< pop à la place?
      go (x:rest) = do
        -- First code tells us the operand
        let c = ord x - 35
            code = if c < 0 then 57 else c
            (n, op, d) = go2 code 0
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
        in if 2+d < n then go2 (n-(d+3)) (op+1) else (n, op, d)

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
  -- write2 symbolTableFst =<< toRibStringl gloName
  -- symtbl=symtbl[1]
  writeRef symbolTablePtr =<< read2 symbolTable
  pure r1

eval :: Rib -> ReaderIO State ()
eval pc = do
  o <- read2 pc
  i <- read1 pc
  stackPtr <- stackRef <$> get
  case i of
    -- jump/call
    RibInt 0 -> do
      traceShowM "jump/call"
      o <- getOpnd o >>= read1
      c <- read1 o
      case c of -- if is_rib(c)
        RibObj ir ir' ir2 -> do
          c2 <- cons (RibInt 0) o -- c2=[0,o,0]
          RibInt arity <- read1 c -- nargs=c[0]
          s2 <- foldrM (\_ args -> pop >>= flip cons args) c2 [1..arity] -- while nargs:s2=[pop(),s2,0];nargs-=1
          read3 pc >>= \case -- if is_rib(pc[2])
            o@RibObj {} -> do -- call
              readRef stackPtr >>= write1 c2 -- c2[0]=stack
              write3 c2 o                    -- c2[2]=pc[2]
            RibInt n -> do -- jump
              k <- getCont          -- k=get_cont()
              read1 k >>= write1 c2 -- c2[0]=k[0]
              read3 k >>= write3 c2 -- c2[2]=k[2]
          writeRef stackPtr s2 -- stack=s2
          read3 c >>= eval -- pc=c[2] & loop

        RibInt n -> do
          primitives !! n -- primitives[c]()
          read3 pc >>= \case -- is_rib(pc[2]):
            RibObj {} -> read3 pc >>= eval -- call. c=pc; pc=c[2]
            RibInt j -> do -- jump
              k <- getCont -- c=get_cont()
              stack <- readRef stackPtr
              read1 k >>= write2 stack -- stack[1]=c[0]
              read3 k >>= eval

    -- set
    RibInt 1 -> do
      traceShowM "set"
      x <- pop
      opnd <- getOpnd o
      write1 opnd x
      read3 pc >>= eval

    -- get
    RibInt 2 -> do
      traceShowM "get"
      getOpnd o >>= read1 >>= push
      read3 pc >>= eval

    -- push
    RibInt 3 -> do
      traceShowM "push"
      push o
      read3 pc >>= eval

    -- if
    RibInt 4 -> do
      traceShowM "if"
      tos <- popFast
      f <- falseRef <$> get
      if tos == f -- IORef Eq's instance is pointer equality.
        then read3 pc >>= eval
        else read2 pc >>= eval

    -- halt
    _ -> do
      traceShowM "HALT!"
      pure ()

  -- printInstrRib pc

getOpnd :: Rib -> ReaderIO State Rib
getOpnd o@RibObj {} = pure o
getOpnd (RibInt n) = do
  stackPtr <- stackRef <$> get
  readRef stackPtr >>= listTail n

-- Look at stack until it finds the continuation rib. The continuation rib is
-- the first rib of the stack that doesn't have an Int as its tag.
getCont :: ReaderIO State Rib
getCont = do
  stackPtr <- stackRef <$> get
  readRef stackPtr >>= go
  where
    go s = case s of
      RibInt _ -> error "getCont: Stack is not a Rib."
      r ->
        read3 r >>= \case
          RibInt _ -> read2 r >>= go
          _ -> pure s

createState :: IO (State, Rib)
createState = do
  -- Creating a partial state to decode the instructions.
  -- We just need a stack and the symbol table.
  -- The global object references will be patched later.
  initialSymbolTable <- initialSymbolTable symbolTableStr emptySymbolsCount
  symbolTableRef <- newRef initialSymbolTable
  stackPtr <- newRef (RibInt 0)
  let state = State stackPtr symbolTableRef (error "Forced true") (error "Forced false") (error "Forced nil")

  -- Decode instructions.
  -- It would be nice if decoding wouldn't execute in ReaderIO State.
  instr <- runReaderIO (decodeInstructions instructionsStr) state

  -- Set global
  setGlobal symbolTableRef "symbtl" =<< mkProc (RibInt 0) symbolTableRef -- primitive 0
  falseRef <- setGlobal symbolTableRef "false" ribFalse
  trueRef <- setGlobal symbolTableRef "true" ribTrue
  nilRef <- setGlobal symbolTableRef "nil" ribNil

  -- Restore symbol table pointer so we don't lose some entries.
  -- This is because setGlobal sets the symbol table pointer to the cdr.
  -- #### TODO: Does it break anything? ###
  -- writeRef symbolTableRef initialSymbolTable

  -- Replace stack with [0,0,[5,0,0]]:
  -- primordial continuation which executes halt instruction.
  halt1 <- mkObj (RibInt 0) (RibInt 5) (RibInt 0)
  halt2 <- mkObj halt1 (RibInt 0) (RibInt 0)
  writeRef stackPtr halt2
  pure (State stackPtr symbolTableRef falseRef trueRef nilRef, instr)
