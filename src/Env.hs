{-# LANGUAGE LambdaCase #-}
module Env where

import Prelude
import Data.IORef

import Rib

data State = State
  { stackRef       :: Rib
  , symbolTableRef :: Rib
  , falseRef       :: Rib
  , trueRef        :: Rib
  , nilRef         :: Rib
  }
