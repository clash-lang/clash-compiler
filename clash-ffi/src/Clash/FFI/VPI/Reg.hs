{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.FFI.VPI.Reg
  ( Reg(..)
  ) where

import Control.DeepSeq (NFData)
import Foreign.Storable (Storable)

import Clash.FFI.VPI.Object

-- | A register is a VPI object that is known to refer to something with the
-- @vpiRegister@ object type.
--
-- Registers can be unsafely created from 'Object', although this is only safe
-- if you first check the @vpiType@ property to confirm it is a register.
-- Registers can be safely converted to the base object type using
-- 'regObject'.
--
newtype Reg
  = Reg { regObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, NFData, Storable)

