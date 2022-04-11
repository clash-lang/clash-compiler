module Clash.FFI.VPI.Parameter
  ( Parameter(..)
  ) where

import Foreign.Storable (Storable)

import Clash.FFI.VPI.Object

newtype Parameter
  = Parameter { parameterObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, Storable)

