module Clash.FFI.VPI.Net
  ( Net(..)
  ) where

import Foreign.Storable (Storable)

import Clash.FFI.VPI.Object

newtype Net
  = Net { netObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, Storable)

