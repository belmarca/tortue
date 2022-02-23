module Env where

import Rib
import Utils
import GHC.IO

{-# NOINLINE stack #-}
stack = unsafePerformIO . newRef $ RibInt 0

-- getStack :: IO Rib -- Debug
getStack = readRef stack

-- setStack :: Rib -> IO () -- Debug
setStack = writeRef stack
