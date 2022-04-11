module Clash.FFI.VPI.Port
  ( Port(..)
  , direction
  , module Clash.FFI.VPI.Port.Direction
  ) where

import Foreign.Storable (Storable)

import Clash.FFI.Monad (SimCont)
import Clash.FFI.VPI.Object
import Clash.FFI.VPI.Port.Direction

newtype Port
  = Port { portObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, Storable)

direction :: Port -> SimCont o Direction
direction = receiveProperty Direction

