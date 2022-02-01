module Env where

import Prelude
import Rib

newtype State = State{ stackRef :: Rib }

emptyState :: State
emptyState=State(RibInt 0)
