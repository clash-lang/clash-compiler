{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.FFI.VPI.Parameter
  ( Parameter(..)
  ) where

import Control.DeepSeq (NFData)
import Foreign.Storable (Storable)

import Clash.FFI.VPI.Object

-- | A parameter is a VPI object that is known to refer to something with the
-- @vpiParameter@ object type.
--
-- Parameters can be unsafely created from 'Object', although this is only safe
-- if you first check the @vpiType@ property to confirm it is a parameter.
-- Parameters can be safely converted to the base object type using
-- 'parameterObject'.
--
newtype Parameter
  = Parameter { parameterObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, NFData, Storable)

