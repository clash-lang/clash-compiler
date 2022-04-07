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
import Clash.FFI.VPI.Handle
import Clash.FFI.VPI.Object
import Clash.FFI.VPI.Port.Direction
import Clash.FFI.VPI.Property

newtype Port
  = Port { portObject :: Object }
  deriving stock (Show)
  deriving newtype (Handle, Storable)

instance HandleObject Port where
  handleAsObject = portObject

portName :: Port -> SimCont o ByteString
portName = receiveProperty Name

portDirection :: HasCallStack => Port -> SimCont o Direction
portDirection = receiveProperty Direction

portIndex :: HasCallStack => Port -> SimCont o CInt
portIndex = getProperty PortIndex

portSize :: HasCallStack => Port -> SimCont o CInt
portSize = getProperty Size

portIsVector :: HasCallStack => Port -> SimCont o Bool
portIsVector = getProperty IsVector

portIsScalar :: HasCallStack => Port -> SimCont o Bool
portIsScalar = getProperty IsScalar

