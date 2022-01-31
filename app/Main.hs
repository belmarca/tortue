module Main where

import Utils
import VM

main :: IO ()
main = do
  (st, instructions) <- createState
  _ <- runReaderIO (eval instructions) st
  pure ()
