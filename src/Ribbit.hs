module Ribbit
  ( ribbitEval,
    ribTrue,
    ribFalse,
    ribNil,
    putchar,
    push,
    pop,
  )
where

import Data.Char (chr, ord)
import GHC.IO.FD (stdin)

ribbitEval :: IO ()
ribbitEval = putStrLn "ribbitEval"

data Rib = RibInt Int | Rib Rib Rib Rib deriving Show

ribTrue :: Rib
ribTrue = Rib (RibInt 0) (RibInt 0) (RibInt 5)

ribFalse :: Rib
ribFalse = Rib (RibInt 0) (RibInt 0) (RibInt 5)

ribNil :: Rib
ribNil = Rib (RibInt 0) (RibInt 0) (RibInt 5)

symbolTable :: Rib
symbolTable = Rib (Rib (RibInt 0) (Rib ribNil (RibInt 0) (RibInt 3)) (RibInt 2)) ribNil (RibInt 0)

putchar :: Int -> IO ()
putchar c = putChar (chr c)

type Stack = [Int]

push :: Stack -> Int -> Stack
push stack val = val : stack

pop :: Stack -> (Int, Stack)
pop stack = (head stack, tail stack)
