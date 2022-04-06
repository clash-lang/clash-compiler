{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Clash.FFI.VPI.Parameter
  ( Parameter(..)
  , ConstantType
      ( DecimalConst
      , RealConst
      , BinaryConst
      , OctalConst
      , HexConst
      , StringConst
#if defined(VERILOG_2001)
      , IntConst
#endif
      , TimeConst
      )
  , parameterName
  , parameterFullName
  , parameterSize
  , parameterType
#if defined(VERILOG_2001)
  , parameterIsLocal
  , parameterIsSigned
#endif
  , parameterValue
  , parameterValueAs
  ) where

import           Data.ByteString (ByteString)
import           Data.Proxy (Proxy)
import           Foreign.C.Types (CInt)
import           GHC.Stack (HasCallStack, callStack)
import           GHC.TypeNats

import           Clash.Promoted.Nat

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.VPI.Object
import           Clash.FFI.VPI.Property
import           Clash.FFI.VPI.Value

newtype Parameter = Parameter { parameterHandle :: Handle }

-- TODO Make this an ADT so it shows nicely.

newtype ConstantType = ConstantType CInt

pattern DecimalConst :: ConstantType
pattern DecimalConst = ConstantType 1

pattern RealConst :: ConstantType
pattern RealConst = ConstantType 2

pattern BinaryConst :: ConstantType
pattern BinaryConst = ConstantType 3

pattern OctalConst :: ConstantType
pattern OctalConst = ConstantType 4

pattern HexConst :: ConstantType
pattern HexConst = ConstantType 5

pattern StringConst :: ConstantType
pattern StringConst = ConstantType 6

#if defined(VERILOG_2001)
pattern IntConst :: ConstantType
pattern IntConst = ConstantType 7
#endif

pattern TimeConst :: ConstantType
pattern TimeConst = ConstantType 8

parameterName :: HasCallStack => Parameter -> SimCont o ByteString
parameterName = receiveProperty Name . parameterHandle

parameterFullName :: HasCallStack => Parameter -> SimCont o ByteString
parameterFullName = receiveProperty FullName . parameterHandle

parameterSize :: (HasCallStack, Integral n) => Parameter -> SimCont o n
parameterSize = fmap fromIntegral . getProperty Size . parameterHandle

parameterType :: HasCallStack => Parameter -> SimCont o ConstantType
parameterType = coerceProperty ConstType . parameterHandle

#if defined(VERILOG_2001)
parameterIsLocal :: HasCallStack => Parameter -> SimCont o Bool
parameterIsLocal = getProperty IsLocalParam . parameterHandle

parameterIsSigned :: HasCallStack => Parameter -> SimCont o Bool
parameterIsSigned = getProperty IsSigned . parameterHandle
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
  receiveValue fmt . parameterHandle

