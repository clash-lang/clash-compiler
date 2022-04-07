{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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

newtype Parameter
  = Parameter { parameterObject :: Object }
  deriving stock (Show)
  deriving newtype (Handle, Storable)

instance HandleObject Parameter where
  handleAsObject = parameterObject

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
parameterValue param = do
  size <- parameterSize param

  case someNatVal size of
    SomeNat (proxy :: Proxy sz) ->
      case compareSNat (SNat @1) (snatProxy proxy) of
        SNatLE -> parameterValueAs ObjTypeFmt param
        SNatGT -> Sim.throw (ZeroWidthValue callStack)

parameterValueAs
  :: HasCallStack
  => ValueFormat
  -> Parameter
  -> SimCont o Value
parameterValueAs fmt =
  receiveValue fmt . parameterObject

