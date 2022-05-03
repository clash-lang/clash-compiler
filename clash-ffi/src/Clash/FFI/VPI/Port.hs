{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.FFI.VPI.Port
  ( Port(..)
  , direction
  , module Clash.FFI.VPI.Port.Direction
  ) where

import Control.DeepSeq (NFData)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)

import Clash.FFI.Monad (SimCont)
import Clash.FFI.VPI.Object
import Clash.FFI.VPI.Port.Direction

-- | A port is a VPI object that is known to refer to something with the
-- @vpiPort@ object type.
--
-- Ports can be unsafely created from 'Object', although this is only safe if
-- you first check the @vpiType@ property to confirm it is a port. Ports can be
-- safely converted to the base object type using 'portObject'.
--
-- Every port in a design is also a net, with different properties available
-- to each. The direction can only be accessed from a port, and the value can
-- only be accessed with a net.
--
newtype Port
  = Port { portObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, NFData, Storable)

-- | The direction of the port, as specified in the design.
--
direction :: Typeable o => Port -> SimCont o Direction
direction = receiveProperty Direction
