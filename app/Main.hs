module Main where

import Debug
import Repl
import Rib
import Utils
import VM

import System.Exit (exitSuccess)

main :: IO ()
main = do
  (st, instructions) <- createState
  runReaderIO (eval instructions) st
  exitSuccess
