{-# LANGUAGE NoMonomorphismRestriction #-}

module Utils where

import Data.IORef

-- newRef :: a -> IO (IORef a) -- Debug
newRef = newIORef

-- readRef :: IORef a -> IO a -- Debug
readRef = readIORef

-- writeRef :: IORef a -> a -> IO () -- Debug
writeRef = writeIORef

-- Instead of using the TupleSection extension, we define a synonym that can be partially applied
-- pair :: a -> b -> (a, b) -- Debug
pair a b = (a,b)
