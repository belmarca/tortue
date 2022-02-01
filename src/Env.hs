module Env where

import Prelude
import Rib
import Utils

newtype State = State { stackRef :: Rib }

emptyState :: State
emptyState=State(RibInt 0)

getStack :: ReaderIO State Rib
getStack = stackRef <$> get

setStack :: Rib -> ReaderIO State ()
setStack newStack = do
  st <- get
  set st {stackRef=newStack}
