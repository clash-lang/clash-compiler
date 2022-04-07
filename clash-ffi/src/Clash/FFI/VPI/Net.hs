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
import           Foreign.Storable (Storable)
import           GHC.Stack (HasCallStack, callStack)
import           GHC.TypeNats

import           Clash.Promoted.Nat

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.VPI.Handle
import           Clash.FFI.VPI.Object
import           Clash.FFI.VPI.Property
import           Clash.FFI.VPI.Value

newtype Net
  = Net { netObject :: Object }
  deriving stock (Show)
  deriving newtype (Handle, Storable)

instance HandleObject Net where
  handleAsObject = netObject

netName :: HasCallStack => Net -> SimCont o ByteString
netName = receiveProperty Name

netFullName :: HasCallStack => Net -> SimCont o ByteString
netFullName = receiveProperty FullName

netSize :: (HasCallStack, Integral n) => Net -> SimCont o n
netSize = fmap fromIntegral . getProperty Size

netIsScalar :: HasCallStack => Net -> SimCont o Bool
netIsScalar = getProperty IsScalar

netIsVector :: HasCallStack => Net -> SimCont o Bool
netIsVector = getProperty IsVector

#if defined(VERILOG_2001)
netIsSigned :: HasCallStack => Net -> SimCont o Bool
netIsSigned = getProperty IsSigned
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
  receiveValue fmt . netObject

