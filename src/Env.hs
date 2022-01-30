{-# LANGUAGE LambdaCase #-}
module Env where

import Prelude
import Data.IORef

import Rib

data State = State
  { stackRef       :: IORef Rib
  , symbolTableRef :: IORef Rib
  , falseRef       :: IORef Rib
  , trueRef        :: IORef Rib
  , nilRef         :: IORef Rib
  }
