module Clash.FFI.VPI.Module
  ( Module(..)
  , topModules
  , moduleNets
  , moduleParameters
  , modulePorts
  , moduleRegs
  ) where

import Foreign.Storable (Storable)
import GHC.Stack (HasCallStack)

import Clash.FFI.Monad (SimCont)
import Clash.FFI.VPI.Iterator
import Clash.FFI.VPI.Object
import Clash.FFI.VPI.Net (Net(..))
import Clash.FFI.VPI.Parameter (Parameter(..))
import Clash.FFI.VPI.Port (Port(..))
import Clash.FFI.VPI.Reg (Reg(..))

newtype Module
  = Module { moduleObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, Storable)

topModules :: HasCallStack => SimCont o [Module]
topModules = iterateAll @_ @Object ObjModule Nothing

moduleNets :: HasCallStack => Module -> SimCont o [Net]
moduleNets = iterateAll ObjNet . Just

moduleParameters :: HasCallStack => Module -> SimCont o [Parameter]
moduleParameters = iterateAll ObjParameter . Just

modulePorts :: HasCallStack => Module -> SimCont o [Port]
modulePorts = iterateAll ObjPort . Just

moduleRegs :: HasCallStack => Module -> SimCont o [Reg]
moduleRegs = iterateAll ObjReg . Just

