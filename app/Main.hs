module Main where

import Control.Monad
import System.Environment

import Utils
import VM

-- inputStr :: String -- Debug
inputStr = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" -- RVM code that prints HELLO!

-- main :: IO () -- Debug
-- main = do
--   (st, instructions) <- createState inputStr
--   void $ runReaderIO (eval instructions) st

-- Main that takes arguments.
-- main :: IO () -- Debug
main = do
  args <- getArgs
  programStr <- case args of
    [file] -> readFile file
    _ -> pure inputStr
  (st, instructions) <- createState programStr
  void $ runReaderIO (eval instructions) st
