{-# LANGUAGE LambdaCase, TupleSections, FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module VM where

import Control.Monad
<<<<<<< HEAD
import Control.Exception
import System.IO.Error
=======
import Control.Monad.IO.Class
>>>>>>> e109d565fe70ed44324694f1ad3e8691411cf861
import Data.Char ( ord, chr )
import Data.Foldable ( foldrM )
import Data.IORef ( IORef )

import Utils
import Rib
import Env

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

type Prim = ReaderIO State ()

push :: ToRib a => a -> ReaderIO State ()
push v = do
  stack <- stackRef <$> get
  -- Cons v to top of stack
  newStack <- cons v stack
  -- Update stack reference to point to new top of stack
  st <- get
  set st {stackRef=newStack}

pop :: ReaderIO State Rib
pop = do
  stack <- stackRef <$> get
  case stack of
    RibInt _ -> error "Empty stack"
    RibRef r ->
      readRef r >>= \(RibObj top rest _) -> do
        -- Update stack reference to point to rest of stack
        st <- get
        set st {stackRef=rest}
        -- Return value
        pure top

-- Primitives

prim1 :: (Rib -> ReaderIO State Rib) -> Prim
prim1 f = do
  v1 <- pop
  f v1 >>= push

prim2 :: (Rib -> Rib -> ReaderIO State Rib) -> Prim
prim2 f = do
  v2 <- pop
  v1 <- pop
  f v1 v2 >>= push

prim3 :: (Rib -> Rib -> Rib -> ReaderIO State Rib) -> Prim
prim3 f = do
  v3 <- pop
  v2 <- pop
  v1 <- pop
  f v1 v2 v3 >>= push

primitives :: [Prim]
primitives = zipWith decorator [0..]
  [ prim3 (\a b c -> toRib $ RibObj a b c)                                            -- rib object constructor
  , prim1 pure                                                                        -- id
  , void pop                                                                          -- take 2 TOS, keep first
  , prim2 (const pure)                                                                -- take 2 TOS, keep second
  , do
      v1 <- pop >>= read0
      stack <- stackRef <$> get
      mkProc v1 stack >>= push                                                     -- close
  , prim1 (pure . (\case RibInt _ -> ribFalse; _ -> ribTrue))                         -- rib?
  , prim1 read0                                                                       -- field0 -- 6
  , prim1 read1                                                                       -- field1 -- 7
  , prim1 read2                                                                       -- field2 -- 8
  , prim2 (\r v -> write0 r v >> pure v)                                              -- field0-set! -- 9
  , prim2 (\r v -> write1 r v >> pure v)                                              -- field1-set! -- 10
  , prim2 (\r v -> write2 r v >> pure v)                                              -- field2-set! -- 11
  , prim2 $ \r1 r2 -> pure $ toBool (r1 == r2)                                        -- eqv?
  , prim2 $ \(RibInt r1) (RibInt r2) -> pure $ toBool (r1 < r2)                       -- <
  , prim2 $ \(RibInt r1) (RibInt r2) -> pure $ RibInt (r1 + r2)                       -- add
  , prim2 $ \(RibInt r1) (RibInt r2) -> pure $ RibInt (r1 - r2)                       -- sub
  , prim2 $ \(RibInt r1) (RibInt r2) -> pure $ RibInt (r1 * r2)                       -- mult
  , prim2 $ \(RibInt r1) (RibInt r2) -> pure $ RibInt (div r1 r2)                     -- quotient
  , liftIO getChar' >>= push . RibInt                                            -- getChar
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
  read0 =<< listTail n symbolTable

listTail :: MonadIO m => Int -> Rib -> m Rib
listTail = \case 0 -> pure; n -> read1 >=> listTail (n-1)

decodeInstructions :: String -> ReaderIO State Rib
decodeInstructions instrStr = do
  let
      go :: String -> ReaderIO State Rib
      go [] = pop
      go (x:rest) = do
        -- First code tells us the operand
        let c = ord x - 35
            code = if c < 0 then 57 else c
            (n, op, d) = go2 code 0
        if c > 90
        then do
          tos <- pop
          stack <- stackRef <$> get
          read0 stack >>=
            mkInstr (op - 1) tos >>=
              write0 stack
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

          stack <- stackRef <$> get
          case stack of
            RibInt i -> read0 n >>= read2 -- End: pc = n[0][2]
            RibRef r -> do
              read0 stack >>=
                mkInstr (min 4 op - 1) n >>=
                  write0 stack
              -- RibObj v1 _ _ <- readRef r
              -- readRef v1 >>=
              --   mkInstr (min 4 op - 1) n >>=
              --     writeRef v1
              go rest'

      -- Finds the op code from the encoded instruction
      go2 n op =
        let d = [20,30,0,10,11,4] !! op
        in if 2+d < n then go2 (n-(d+3)) (op+1) else (n, op, d)

  go instrStr

-- FIXME: We lose the elements of the symbol table.
-- It would be nice if they were kept and available to display when debugging.
-- For now, the symbol table reference is restored after the calls to setGlobal.
setGlobal :: String -> Rib -> ReaderIO State () -- (IORef Rib)
setGlobal gloName val = do
  symbolTable <- symbolTableRef <$> get
  -- symtbl[0][0]=val
  symbolTableFst <- read0 symbolTable
  write0 symbolTableFst val
  -- Give name to global variable.
  -- Equivalent to symtbl[0][1]=val
  -- Note: This is not in the python implementation
  write1 symbolTableFst =<< toRibString gloName
  -- symtbl=symtbl[1]
  s1 <- read1 symbolTable
  st <- get
  set st {symbolTableRef=s1}

eval :: Rib -> ReaderIO State ()
eval pc = do
  o <- read1 pc
  i <- read0 pc
  case i of
    -- jump/call
    RibInt 0 -> do
      -- traceShowM "jump/call"
      o <- getOpnd o >>= read0
      c <- read0 o
      case c of -- if is_rib(c)
        RibRef r -> do
          c2 <- cons (RibInt 0) o -- c2=[0,o,0]
          RibInt arity <- read0 c -- nargs=c[0]
          s2 <- foldrM (\_ args -> pop >>= flip cons args) c2 [1..arity] -- while nargs:s2=[pop(),s2,0];nargs-=1
          read2 pc >>= \case -- if is_rib(pc[2])
            o@RibRef {} -> do -- call
              stack <- stackRef <$> get
              write0 c2 stack -- c2[0]=stack
              write2 c2 o     -- c2[2]=pc[2]
            RibInt n -> do -- jump
              k <- getCont          -- k=get_cont()
              write0 c2 =<< read0 k -- c2[0]=k[0]
              write2 c2 =<< read2 k -- c2[2]=k[2]

          st <- get
          set st {stackRef=s2} -- stack=s2
          read2 c >>= eval -- pc=c[2] & loop

        RibInt n -> do
          primitives !! n -- primitives[c]()
          read2 pc >>= \case -- is_rib(pc[2]):
            RibRef _ -> read2 pc >>= eval -- call. c=pc; pc=c[2]
            RibInt j -> do -- jump
              k <- getCont -- c=get_cont()
              stack <- stackRef <$> get
              read0 k >>= write1 stack -- stack[1]=c[0]
              read2 k >>= eval

    -- set
    RibInt 1 -> do
      -- traceShowM "set"
      x <- pop
      opnd <- getOpnd o
      write0 opnd x
      read2 pc >>= eval

    -- get
    RibInt 2 -> do
      -- traceShowM "get"
      getOpnd o >>= read0 >>= push
      read2 pc >>= eval

    -- push
    RibInt 3 -> do
      -- traceShowM "push"
      push o
      read2 pc >>= eval

    -- if
    RibInt 4 -> do
      -- traceShowM "if"
      tos <- pop
      f <- falseRef <$> get
      -- IORef Eq's instance is pointer equality.
      if tos == f
        then read2 pc >>= eval
        else read1 pc >>= eval

    -- halt
    _ -> do
      -- traceShowM "HALT!"
      pure ()

getOpnd :: Rib -> ReaderIO State Rib
getOpnd (RibInt n) = get >>= listTail n . stackRef
getOpnd o = pure o

-- Look at stack until it finds the continuation rib. The continuation rib is
-- the first rib of the stack that doesn't have an Int as its tag.
getCont :: ReaderIO State Rib
getCont = do
  get >>= go . stackRef
  where
    go s = case s of
      RibInt _ -> error "getCont: Stack is not a Rib."
      r ->
        read2 r >>= \case
          RibInt _ -> read1 r >>= go
          _ -> pure s

createState :: IO (State, Rib)
createState = do
  -- Creating a partial state to decode the instructions.
  -- We just need a stack and the symbol table.
  -- The global object references will be patched later.
  symbolTable <- initialSymbolTable symbolTableStr emptySymbolsCount
  let stack = RibInt 0
      state = State stack symbolTable ribFalse ribTrue ribNil

  -- Decode instructions.
  -- It would be nice if decoding wouldn't execute in ReaderIO State.
  flip runReaderIO state $ do
    instr <- decodeInstructions instructionsStr

    -- Set global
    setGlobal "symbtl" =<< mkProc (RibInt 0) symbolTable -- primitive 0
    setGlobal "false" ribFalse
    setGlobal "true" ribTrue
    setGlobal "nil" ribNil

    -- Restore symbol table pointer so we don't lose some entries.
    -- This is because setGlobal sets the symbol table pointer to the cdr.
    -- #### TODO: Does it break anything? ###
    st <- get
    set st {symbolTableRef=symbolTable}

    -- Replace stack with [0,0,[5,0,0]]:
    -- primordial continuation which executes halt instruction.
    halt1 <- mkObj (RibInt 0) (RibInt 5) (RibInt 0)
    halt2 <- mkObj halt1 (RibInt 0) (RibInt 0)
    st <- get
    set st {stackRef=halt2}
    pure instr

getChar' :: IO Int
getChar' = (ord <$> getChar) `catch` eofHandler
  where
    eofHandler e = if isEOFError e then return (-1) else return (-2)
