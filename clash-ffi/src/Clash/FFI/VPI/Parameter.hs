{-# LANGUAGE CPP #-}

module Clash.FFI.VPI.Parameter
  ( Parameter(..)
  , parameterName
  , parameterFullName
  , parameterSize
#if defined(VERILOG_2001)
  , parameterIsLocal
  , parameterIsSigned
#endif
  , parameterValue
  , parameterValueAs
  ) where

import Data.ByteString (ByteString)
import Foreign.Storable (Storable)
import GHC.Stack (HasCallStack)

import Clash.FFI.Monad (SimCont)
import Clash.FFI.VPI.Object

newtype Parameter
  = Parameter { parameterObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, Storable)

parameterName :: HasCallStack => Parameter -> SimCont o ByteString
parameterName = receiveProperty Name

parameterFullName :: HasCallStack => Parameter -> SimCont o ByteString
parameterFullName = receiveProperty FullName

parameterSize :: (HasCallStack, Integral n) => Parameter -> SimCont o n
parameterSize = fmap fromIntegral . getProperty Size

#if defined(VERILOG_2001)
parameterIsLocal :: HasCallStack => Parameter -> SimCont o Bool
parameterIsLocal = getProperty IsLocalParam

parameterIsSigned :: HasCallStack => Parameter -> SimCont o Bool
parameterIsSigned = getProperty IsSigned
#endif

parameterValue :: HasCallStack => Parameter -> SimCont o Value
parameterValue = parameterValueAs ObjTypeFmt

parameterValueAs
  :: HasCallStack
  => ValueFormat
  -> Parameter
  -> SimCont o Value
parameterValueAs fmt =
  receiveValue fmt

