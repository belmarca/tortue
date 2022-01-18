{-# LANGUAGE LambdaCase, Strict #-}
module Repl where

import Debug
import Utils
import VM
import Control.Monad (forM, forM_, replicateM, replicateM_)

createState :: IO State
createState = do
  stack <- newRef ribNil
  symbolTable <- initialSymbolTable'
  State <$> newRef ribNil <*> newRef symbolTable

--
runWithState :: ReaderIO State a -> IO State
runWithState prog = do
  state <- createState
  runReaderIO prog state
  pure state

-- To run program, call `run` function
prog :: ReaderIO State ()
prog = do
  let msg = "HELLO, WORLD!"
  -- On pousse le message en ordre inverse
  forM_ (reverse msg) push
  -- On appelle put char et on enlève le charactère du stack
  replicateM_ (length msg) (callPrim 18 >> void pop)
  where
    callPrim :: Int -> ReaderIO State ()
    callPrim primCode = do
      primitives !! primCode


run :: IO ()
run = runWithState prog >>= printState
