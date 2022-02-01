module Env where

import Rib
import Utils

newtype State = State { stackRef :: Rib }

type SIO = StateIO State

-- emptyState :: State -- Debug
emptyState = State (RibInt 0)

-- getStack :: SIO Rib -- Debug
getStack = stackRef <$> get

-- setStack :: Rib -> SIO () -- Debug
setStack = set . State
