{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.FFI.VPI.Net
  ( Net(..)
  ) where

import Control.DeepSeq (NFData)
import Foreign.Storable (Storable)

import Clash.FFI.VPI.Object

-- | A net is a VPI object that is known to refer to something with the
-- @vpiNet@ object type.
--
-- Nets can be unsafely created from 'Object', although this is only safe if
-- you first check the @vpiType@ property to confirm it is a net. Nets can be
-- safely converted to the base object type using 'netObject'.
--
newtype Net
  = Net { netObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, NFData, Storable)
