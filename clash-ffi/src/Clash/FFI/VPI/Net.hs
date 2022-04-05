{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Clash.FFI.VPI.Net
  ( Net(..)
  , netName
  , netFullName
  , netSize
  , netIsScalar
  , netIsVector
#if defined(VERILOG_2001)
  , netIsSigned
#endif
  , netValue
  , netValueAs
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

newtype Net = Net { netHandle :: Handle }

netName :: HasCallStack => Net -> SimCont o ByteString
netName = receiveProperty VpiName . netHandle

netFullName :: HasCallStack => Net -> SimCont o ByteString
netFullName = receiveProperty VpiFullName . netHandle

netSize :: (HasCallStack, Integral n) => Net -> SimCont o n
netSize = fmap fromIntegral . getProperty VpiSize . netHandle

netIsScalar :: HasCallStack => Net -> SimCont o Bool
netIsScalar = getProperty VpiScalar . netHandle

netIsVector :: HasCallStack => Net -> SimCont o Bool
netIsVector = getProperty VpiVector . netHandle

#if defined(VERILOG_2001)
netIsSigned :: HasCallStack => Net -> SimCont o Bool
netIsSigned = getProperty VpiSigned . netHandle
#endif

netValue :: HasCallStack => Net -> SimCont o Value
netValue net = do
  size <- netSize net

  case someNatVal size of
    SomeNat (proxy :: Proxy sz) ->
      case compareSNat (SNat @1) (snatProxy proxy) of
        SNatLE -> netValueAs ObjTypeFmt net
        SNatGT -> Sim.throw (ZeroWidthValue callStack)

netValueAs
  :: HasCallStack
  => ValueFormat
  -> Net
  -> SimCont o Value
netValueAs fmt =
  receiveValue fmt . netHandle

