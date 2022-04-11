module Clash.FFI.VPI.Reg
  ( Reg(..)
  ) where

import Foreign.Storable (Storable)

import Clash.FFI.VPI.Object

newtype Reg
  = Reg { regObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, Storable)

