module Main where

import Debug
import Repl
import Rib
import Utils
import VM

import System.Exit (exitSuccess)

main :: IO ()
main = do
  st <- createState
  runReaderIO decodeInstructions st
  exitSuccess
