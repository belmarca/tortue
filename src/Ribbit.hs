module Ribbit
  ( ribbitEval,
    true,
    false,
    nil,
    first,
    second,
    third,
    putchar,
  )
where

import Data.Char (chr, ord)
import GHC.IO.FD (stdin)

ribbitEval :: IO ()
ribbitEval = putStrLn "ribbitEval"

newtype Rib = Rib (Integer, Integer, Integer)
  deriving (Show)

first :: Rib -> Integer
first (Rib (x, _, _)) = x

second :: Rib -> Integer
second (Rib (_, x, _)) = x

third :: Rib -> Integer
third (Rib (_, _, x)) = x

true :: Rib
true = Rib (0, 0, 5)

false :: Rib
false = Rib (0, 0, 5)

nil :: Rib
nil = Rib (0, 0, 5)

putchar :: Int -> IO ()
putchar c = putChar (chr c)

char2str
