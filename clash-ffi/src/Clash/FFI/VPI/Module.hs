module Clash.FFI.VPI.Module
  ( Module(..)
  , topModules
  , moduleName
  , moduleFullName
  , moduleNets
  , moduleParameters
  , modulePorts
  , moduleRegs
  ) where

import Data.ByteString (ByteString)
import GHC.Stack (HasCallStack)

import Clash.FFI.Monad (SimCont)
import Clash.FFI.VPI.Object
import Clash.FFI.VPI.Net (Net(..))
import Clash.FFI.VPI.Parameter (Parameter(..))
import Clash.FFI.VPI.Port (Port(..))
import Clash.FFI.VPI.Property
import Clash.FFI.VPI.Reg (Reg(..))

newtype Module = Module { moduleHandle :: Handle }

topModules :: HasCallStack => SimCont o [Module]
topModules =
  fmap Module <$> iterateHandle ObjModule Nothing

moduleName :: HasCallStack => Module -> SimCont o ByteString
moduleName = receiveProperty VpiName . moduleHandle

moduleFullName :: HasCallStack => Module -> SimCont o ByteString
moduleFullName = receiveProperty VpiFullName . moduleHandle

moduleNets :: HasCallStack => Module -> SimCont o [Net]
moduleNets =
  fmap (fmap Net) . iterateHandle ObjNet . Just . moduleHandle

moduleParameters :: HasCallStack => Module -> SimCont o [Parameter]
moduleParameters =
  fmap (fmap Parameter) . iterateHandle ObjParameter . Just . moduleHandle

modulePorts :: HasCallStack => Module -> SimCont o [Port]
modulePorts =
  fmap (fmap Port) . iterateHandle ObjPort . Just . moduleHandle

moduleRegs :: HasCallStack => Module -> SimCont o [Reg]
moduleRegs =
  fmap (fmap Reg) . iterateHandle ObjReg . Just . moduleHandle

