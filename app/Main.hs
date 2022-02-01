module Main where

import Control.Monad

import Utils
import VM

-- inputStr :: String -- Debug
inputStr = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" -- RVM code that prints HELLO!

-- main :: IO () -- Debug
main = do
  (st, instructions) <- createState inputStr
  void $ runReaderIO (eval instructions) st
