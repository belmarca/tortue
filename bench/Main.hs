module Main where

import Prelude

import Debug
-- import Repl
import Rib
import Utils
import VM

import Criterion
import Criterion.Main

main :: IO ()
main = do
  symTbl <- initialSymbolTable hwSymbolTableStr hwEmptySymbolsCount
  defaultMain
    [ bgroup "RVM"
      [ bgroup "decoding symbol table"
        [ bench "HELLO!" . whnfIO $ initialSymbolTable hwSymbolTableStr hwEmptySymbolsCount
        ]
      , bgroup "decoding instructions"
        [ bench "HELLO!" . whnfIO $ do
            state <- State <$> newRef ribNil <*> newRef symTbl
            runReaderIO (decodeInstructions hwInstructionsStr) state
        ]
      ]
    ]

splitBytecode :: String -> (String, Int, String)
splitBytecode inputStr =
  let (start, end) = span (/= ';') inputStr
      (symTbl, emptySymCount) = readInt start 0
  in (symTbl, emptySymCount, drop 1 end)

helloWorldStr :: String
helloWorldStr = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" -- RVM code that prints HELLO!

hwEmptySymbolsCount :: Int
hwSymbolTableStr, hwInstructionsStr :: String
(hwSymbolTableStr, hwEmptySymbolsCount, hwInstructionsStr) = splitBytecode helloWorldStr
