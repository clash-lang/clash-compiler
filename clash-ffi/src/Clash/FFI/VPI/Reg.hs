{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Reg
  ( Reg(..)
  , regName
  , regFullName
  , regSize
  , regIsScalar
  , regIsVector
#if defined(VERILOG_2001)
  , regIsSigned
#endif
  , regValue
  , regValueAs
  ) where

import Data.ByteString (ByteString)
import Foreign.Storable (Storable)
import GHC.Stack (HasCallStack)

import Clash.FFI.Monad (SimCont)
import Clash.FFI.VPI.Handle
import Clash.FFI.VPI.Object
import Clash.FFI.VPI.Property
import Clash.FFI.VPI.Value

newtype Reg
  = Reg { regObject :: Object }
  deriving stock (Show)
  deriving newtype (Handle, Storable)

regName :: HasCallStack => Reg -> SimCont o ByteString
regName = receiveProperty Name

regFullName :: HasCallStack => Reg -> SimCont o ByteString
regFullName = receiveProperty FullName

regIsScalar :: HasCallStack => Reg -> SimCont o Bool
regIsScalar = getProperty IsScalar

regIsVector :: HasCallStack => Reg -> SimCont o Bool
regIsVector = getProperty IsVector

#if defined(VERILOG_2001)
regIsSigned :: HasCallStack => Reg -> SimCont o Bool
regIsSigned = getProperty IsSigned
#endif

regSize :: HasCallStack => Integral n => Reg -> SimCont o n
regSize = fmap fromIntegral . getProperty Size

regValue :: HasCallStack => Reg -> SimCont o Value
regValue = regValueAs ObjTypeFmt

regValueAs
  :: HasCallStack
  => ValueFormat
  -> Reg
  -> SimCont o Value
regValueAs fmt =
  receiveValue fmt

