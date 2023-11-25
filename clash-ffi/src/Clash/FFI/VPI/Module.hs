{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.FFI.VPI.Module
  ( Module(..)
  , topModules
  , findTopModule
  , moduleNets
  , moduleParameters
  , modulePorts
  , moduleRegs
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Foreign.Storable (Storable)
import GHC.Stack (HasCallStack)

import Clash.FFI.View (ensureNullTerminated)
import Clash.FFI.VPI.Iterator
import Clash.FFI.VPI.Object
import Clash.FFI.VPI.Net (Net(..))
import Clash.FFI.VPI.Parameter (Parameter(..))
import Clash.FFI.VPI.Port (Port(..))
import Clash.FFI.VPI.Reg (Reg(..))

-- | A module is a VPI object that is known to refer to something with the
-- @vpiModule@ object type. This wrapper exposes operations which are known to
-- be safe on objects specifically.
--
-- Modules can be unsafely created from 'Object', although this is only safe if
-- you first check the @vpiType@ property to confirm it is a module. Modules
-- can be safely converted to the base object type using 'moduleObject'.
--
newtype Module
  = Module { moduleObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, NFData, Storable)

-- | Iterate the top-level of a design, finding all the modules.
--
topModules :: HasCallStack => IO [Module]
topModules = iterateAll @_ @Object ObjModule Nothing

-- | Find a top-level module in a design by name. This throws an 'UnknownChild'
-- exception if no top-level module with the given name is found in the design.
--
findTopModule :: HasCallStack => ByteString -> IO Module
findTopModule name =
  unsafeSendChildRef @_ @Object (ensureNullTerminated name) Nothing

-- | Iterate all the nets in a module. This will iterate all nets at once, for
-- large designs it may be more efficient to use
-- 'Clash.FFI.VPI.Iterator.iterate' and 'scan' manually.
--
moduleNets :: HasCallStack => Module -> IO [Net]
moduleNets = iterateAll ObjNet . Just

-- | Iterate all the parameters in a module. This will iterate all nets at
-- once, for large designs it may be more efficient to use
-- 'Clash.FFI.VPI.Iterator.iterate' and 'scan' manually.
--
moduleParameters :: HasCallStack => Module -> IO [Parameter]
moduleParameters = iterateAll ObjParameter . Just

-- | Iterate all the ports in a module. This will iterate all nets at once, for
-- large designs it may be more efficient to use
-- 'Clash.FFI.VPI.Iterator.iterate' and 'scan' manually.
--
modulePorts :: HasCallStack => Module -> IO [Port]
modulePorts = iterateAll ObjPort . Just

-- | Iterate all the registers in a module. This will iterate all nets at once,
-- for large designs it may be more efficient to use
-- 'Clash.FFI.VPI.Iterator.iterate' and 'scan' manually.
--
moduleRegs :: HasCallStack => Module -> IO [Reg]
moduleRegs = iterateAll ObjReg . Just
