{-# LANGUAGE LambdaCase #-}
module Repl where

import Prelude hiding (drop)

import Control.Exception ( bracket )
import Control.Monad (forM, forM_, replicateM, replicateM_, void)
import Data.Char (chr)

import Debug
import Env
import Rib
import Utils
import VM

dummyState :: IO State
dummyState = do
  initialSymbolTable <- initialSymbolTable symbolTableStr emptySymbolsCount
  symbolTableRef <- newRef initialSymbolTable
  stackPtr <- newRef (RibInt 0)
  pure $ State stackPtr symbolTableRef (error "Forced true") (error "Forced false") (error "Forced nil")

-- Helper functions

car, cdr, dup, drop, inc, dec, mkRib :: ReaderIO State ()
car   = callPrim 6
cdr   = callPrim 7
dup   = callPrim 19
drop  = void pop
inc   = push (RibInt 1) >> callPrim 13
dec   = push (RibInt 1) >> swap >> callPrim 14
mkRib = callPrim 0

-- Swap 2 top elements of the stack
swap :: ReaderIO State ()
swap = do
  tos1 <- pop
  tos2 <- pop
  push tos1
  push tos2

callPrim :: Int -> ReaderIO State ()
callPrim primCode =
  primitives !! primCode

-- Basic functions

-- Affiche un string Haskell
progTrace :: String -> ReaderIO State ()
progTrace msg = do
  -- On pousse le message en ordre inverse
  forM_ (reverse msg) push
  -- On appelle put char et on enlève le charactère du stack
  replicateM_ (length msg) (callPrim 18 >> drop)

-- Affiche un string rib
progPrint :: ReaderIO State ()
progPrint = do
  car -- get chars
  printChars
  where
    printChars :: ReaderIO State ()
    printChars = do
      dup -- duplicate pair
      pair <- pop
      -- Is empty?
      case pair of
        -- Nil
        RibInt _ -> drop -- drop nil from stack
        -- Is pair
        _ -> do
          dup         -- duplicate pair
          car         -- get car
          callPrim 18 -- put char
          drop        -- drop char from stack
          cdr         -- get cdr (rest of string)
          printChars  -- start over

-- Traverse la liste sur le TOS et empile sa longueur sur le stack
listLength :: ReaderIO State ()
listLength = do
  -- acc = -1 because we count the first indirection even if it's not part of the list
  push (RibInt (-1)) --
  -- Move acc under list
  swap
  -- Iterate on list and compute length
  go
  -- Remove list from stack
  drop
  where
    go :: ReaderIO State ()
    go = do
      dup -- duplicate pair
      pair <- pop -- Move copy of pair to register
      -- Is empty?
      case pair of
        -- Nil
        RibInt _ -> pure () -- drop nil from stack
        -- Is pair
        _ -> do
          -- Increment accumulator
          swap
          inc
          swap
          cdr -- get cdr (rest of list)
          go  -- start over

listReverse :: ReaderIO State ()
listReverse = do  -- [list]
  push ribNil     -- acc = ribNil [acc, list]
  swap            -- [list, acc]
  go              -- [list, acc]
  drop            -- [acc]
  where
    go :: ReaderIO State ()
    go = do
      dup         -- duplicate pair [list, list, acc]
      pair <- pop -- Move copy of pair to register [list, acc]
      -- Is empty?
      case pair of
        -- Nil
        RibInt _ -> pure () -- drop nil from stack
        -- Is pair
        _ -> do
          dup             -- [list, list, acc]
          car             -- [car, list, acc]
          ca <- pop       -- [list, acc]
          swap            -- [acc, list]
          push (RibInt 0) -- [0, acc, list]
          swap            -- [acc, 0, list]
          push ca         -- [car, acc, 0, list]
          mkRib           -- [car : acc, list]
          -- liftIO . printRibList =<< readRef . stackRef =<< get
          swap            -- [list, car : acc]
          cdr             -- [cdr list, car : acc]
          go              -- start over

askInput :: ReaderIO State ()
askInput = do
  askChars
  listReverse
  packString

packString :: ReaderIO State ()
packString = do
  -- Place tag at bottom
  push (RibInt 3)
  swap
  -- Compute length
  dup
  listLength
  -- Move characters to TOS
  swap
  -- Pack string
  mkRib

askChars :: ReaderIO State ()
askChars = do
  push ribNil -- Initialize list of characters [acc]
  go
  where
    go :: ReaderIO State ()
    go = do
      callPrim 17         -- getChar [char, acc]
      RibInt code <- pop  -- [acc]
      let c = chr code
      if c == '\n'
        then pure ()      -- [acc]
        else do
          push (RibInt 0) -- [0, acc]
          swap            -- [char, acc, 0]
          push code       -- [char, acc, 0]
          mkRib
          go

-- Program

-- To run program, use `run` or `runVerbose` functions.
prog :: ReaderIO State ()
prog = do
  progTrace "HELLO, WORLD!\n"

  -- Demande à l'utilisateur son nom
  push =<< toRibString "Quel est ton nom?\n"
  progPrint

  -- Prend en entrée le nom
  askInput

  -- On salue l'utilisateur
  push =<< toRibString "Allo "
  progPrint -- Affiche "Allo "
  progPrint -- Affiche le nom
  push =<< toRibString "\n"
  progPrint -- Affiche "\n"

-- Affiche le stack courant
inspectStack :: ReaderIO State ()
inspectStack = printRibList =<< readRef . stackRef =<< get

run :: IO ()
run = void (createState >>= runReaderIO prog . fst)

-- Affiche state après exécution.
-- En cas d'exception, affiche le stack au moment de l'échec.
runVerbose :: IO ()
runVerbose = bracket createState (runReaderIO printState . fst) (runReaderIO prog . fst)
  -- void $ runWithState (prog >> printState)
