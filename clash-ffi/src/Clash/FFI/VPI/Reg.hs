{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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

import           Data.ByteString (ByteString)
import           Data.Proxy (Proxy)
import           GHC.Stack (HasCallStack, callStack)
import           GHC.TypeNats

import           Clash.Promoted.Nat

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.VPI.Object
import           Clash.FFI.VPI.Property
import           Clash.FFI.VPI.Value

newtype Reg = Reg { regHandle :: Handle }

regName :: HasCallStack => Reg -> SimCont o ByteString
regName = receiveProperty VpiName . regHandle

regFullName :: HasCallStack => Reg -> SimCont o ByteString
regFullName = receiveProperty VpiFullName . regHandle

regIsScalar :: HasCallStack => Reg -> SimCont o Bool
regIsScalar = getProperty VpiScalar . regHandle

regIsVector :: HasCallStack => Reg -> SimCont o Bool
regIsVector = getProperty VpiVector . regHandle

#if defined(VERILOG_2001)
regIsSigned :: HasCallStack => Reg -> SimCont o Bool
regIsSigned = getProperty VpiSigned . regHandle
#endif

regSize :: (HasCallStack, Integral n) => Reg -> SimCont o n
regSize = fmap fromIntegral . getProperty VpiSize . regHandle

regValue :: HasCallStack => Reg -> SimCont o Value
regValue reg = do
  size <- regSize reg

  case someNatVal size of
    SomeNat (proxy :: Proxy sz) ->
      case compareSNat (SNat @1) (snatProxy proxy) of
        SNatLE -> regValueAs ObjTypeFmt reg
        SNatGT -> Sim.throw (ZeroWidthValue callStack)

regValueAs
  :: HasCallStack
  => ValueFormat
  -> Reg
  -> SimCont o Value
regValueAs fmt =
  receiveValue fmt . regHandle

