{-# LANGUAGE PatternSynonyms #-}

module Clash.FFI.VPI.Port
  ( Port(..)
  , Direction
      ( VpiInput
      , VpiOutput
      , VpiInOut
      , VpiMixedIO
      , VpiNoDirection
      )
  , portName
  , portDirection
  , portIndex
  , portSize
  , portIsVector
  , portIsScalar
  ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt)
import GHC.Stack (HasCallStack)

import Clash.FFI.Monad (SimCont)
import Clash.FFI.VPI.Object (Handle)
import Clash.FFI.VPI.Property

newtype Port = Port { portHandle :: Handle }

portName :: Port -> SimCont o ByteString
portName = receiveProperty VpiName . portHandle

newtype Direction = Direction CInt

pattern VpiInput :: Direction
pattern VpiInput = Direction 1

pattern VpiOutput :: Direction
pattern VpiOutput = Direction 2

pattern VpiInOut :: Direction
pattern VpiInOut = Direction 3

pattern VpiMixedIO :: Direction
pattern VpiMixedIO = Direction 4

pattern VpiNoDirection :: Direction
pattern VpiNoDirection = Direction 5

portDirection :: HasCallStack => Port -> SimCont o Direction
portDirection = coerceProperty VpiDirection . portHandle

portIndex :: HasCallStack => Port -> SimCont o CInt
portIndex = getProperty VpiPortIndex . portHandle

portSize :: HasCallStack => Port -> SimCont o CInt
portSize = getProperty VpiSize . portHandle

portIsVector :: HasCallStack => Port -> SimCont o Bool
portIsVector = getProperty VpiVector . portHandle

portIsScalar :: HasCallStack => Port -> SimCont o Bool
portIsScalar = getProperty VpiScalar . portHandle

