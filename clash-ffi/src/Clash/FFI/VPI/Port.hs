module Clash.FFI.VPI.Port
  ( Port(..)
  , portName
  , portDirection
  , portIndex
  , portSize
  , portIsVector
  , portIsScalar
  , module Clash.FFI.VPI.Port.Direction
  ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt)
import Foreign.Storable (Storable)
import GHC.Stack (HasCallStack)

import Clash.FFI.Monad (SimCont)
import Clash.FFI.VPI.Object (Handle)
import Clash.FFI.VPI.Port.Direction
import Clash.FFI.VPI.Property

newtype Port
  = Port { portHandle :: Handle }
  deriving stock (Show)
  deriving newtype (Storable)

portName :: Port -> SimCont o ByteString
portName = receiveProperty Name . portHandle

portDirection :: HasCallStack => Port -> SimCont o Direction
portDirection = receiveProperty Direction . portHandle

portIndex :: HasCallStack => Port -> SimCont o CInt
portIndex = getProperty PortIndex . portHandle

portSize :: HasCallStack => Port -> SimCont o CInt
portSize = getProperty Size . portHandle

portIsVector :: HasCallStack => Port -> SimCont o Bool
portIsVector = getProperty IsVector . portHandle

portIsScalar :: HasCallStack => Port -> SimCont o Bool
portIsScalar = getProperty IsScalar . portHandle

