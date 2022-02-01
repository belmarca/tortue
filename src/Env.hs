module Env where

import Rib
import Utils

newtype State = State { stackRef :: Rib }

type SIO = StateIO State

emptyState :: State
emptyState=State(RibInt 0)

getStack :: SIO Rib
getStack = stackRef <$> get

setStack :: Rib -> SIO ()
setStack newStack = do
  st <- get
  set st {stackRef=newStack}
