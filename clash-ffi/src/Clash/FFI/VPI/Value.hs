{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Clash.FFI.VPI.Value
  ( CValue(..)
  , Value(..)
  , SomeValue(..)
  , getValue
  , unsafeSendValue
  , sendValue
  , unsafeReceiveValue
  , receiveValue
  , InvalidFormat(..)
  , InvalidValue(..)

  , module Clash.FFI.VPI.Value.Format
  , module Clash.FFI.VPI.Value.Scalar
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
  , module Clash.FFI.VPI.Value.Vector
#endif
  ) where

import           Control.Exception (Exception)
import qualified Control.Exception as IO (throwIO)
import qualified Control.Monad as Monad (void)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.ByteString (ByteString)
import           Data.Proxy (Proxy(..))
import           Data.Type.Equality ((:~:)(..))
import           Foreign.C.String (CString)
import           Foreign.C.Types (CDouble, CInt(..))
import           Foreign.Ptr (Ptr)
import           Foreign.Storable as FFI (Storable(..))
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import           GHC.TypeNats (KnownNat, type (<=), natVal, sameNat)

import           Clash.Sized.BitVector (Bit, BitVector)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (heapPtr, stackPtr, throw, withNewPtr)
import           Clash.FFI.View
import           Clash.FFI.VPI.Object (Handle(..))
import           Clash.FFI.VPI.Time (CTime, Time)
import           Clash.FFI.VPI.Value.Delay
import           Clash.FFI.VPI.Value.Format
import           Clash.FFI.VPI.Value.Scalar

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
import           Clash.FFI.VPI.Value.Vector
#endif

data CValue n where
  CBinStrVal :: CString -> CValue n
  COctStrVal :: CString -> CValue n
  CDecStrVal :: CString -> CValue n
  CHexStrVal :: CString -> CValue n
  CScalarVal :: CInt -> CValue 1
  CIntVal :: CInt -> CValue 32
#if defined(IVERILOG)
  CRealVal :: CDouble -> CValue 1
#else
  CRealVal :: CDouble -> CValue 64
#endif
  CStringVal :: CString -> CValue n
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
  CVectorVal :: Ptr CVector -> CValue n
#endif
  CTimeVal :: Ptr CTime -> CValue 64
  CMiscVal :: CString -> CValue n

deriving stock instance Show (CValue n)

data InvalidFormat where
  InvalidFormat :: ValueFormat n -> CallStack -> InvalidFormat

instance Show InvalidFormat where
  show (InvalidFormat f c) =
    mconcat
      [ "The value format "
      , show f
      , " can not be used in all calls.\n"
      , "Please consult the (System)Verilog specification for details.\n"
      , prettyCallStack c
      ]

deriving anyclass instance Exception InvalidFormat

data InvalidValue where
  InvalidValue :: CValue n -> CallStack -> InvalidValue

instance Show InvalidValue where
  show (InvalidValue v c) =
    mconcat
      [ "Attempt to send/receive a value "
      , show v
      , " which has a format with no data.\n"
      , prettyCallStack c
      ]

deriving anyclass instance Exception InvalidValue

instance (KnownNat n, 1 <= n) => Storable (CValue n) where
  sizeOf _ = 16
  alignment _ = 8

  peek ptr =
    FFI.peekByteOff @(ValueFormat n) ptr 0 >>= \case
      BinStrFmt ->
        CBinStrVal <$> FFI.peekByteOff ptr 8

      OctStrFmt ->
        COctStrVal <$> FFI.peekByteOff ptr 8

      DecStrFmt ->
        CDecStrVal <$> FFI.peekByteOff ptr 8

      HexStrFmt ->
        CHexStrVal <$> FFI.peekByteOff ptr 8

      ScalarFmt ->
        CScalarVal <$> FFI.peekByteOff ptr 8

      IntFmt ->
        CIntVal <$> FFI.peekByteOff ptr 8

      RealFmt ->
        CRealVal <$> FFI.peekByteOff ptr 8

      StringFmt ->
        CStringVal <$> FFI.peekByteOff ptr 8

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
      VectorFmt ->
        CVectorVal <$> FFI.peekByteOff ptr 8
#endif

      TimeFmt ->
        CTimeVal <$> FFI.peekByteOff ptr 8

      fmt ->
        IO.throwIO (InvalidFormat fmt callStack)

  poke ptr = \case
    CBinStrVal bin ->
      FFI.pokeByteOff ptr 0 (BinStrFmt @n) *> FFI.pokeByteOff ptr 8 bin

    COctStrVal oct ->
      FFI.pokeByteOff ptr 0 (OctStrFmt @n) *> FFI.pokeByteOff ptr 8 oct

    CDecStrVal dec ->
      FFI.pokeByteOff ptr 0 (DecStrFmt @n) *> FFI.pokeByteOff ptr 8 dec

    CHexStrVal hex ->
      FFI.pokeByteOff ptr 0 (HexStrFmt @n) *> FFI.pokeByteOff ptr 8 hex

    CScalarVal scalar ->
      FFI.pokeByteOff ptr 0 ScalarFmt *> FFI.pokeByteOff ptr 8 scalar

    CIntVal int ->
      FFI.pokeByteOff ptr 0 IntFmt *> FFI.pokeByteOff ptr 8 int

    CRealVal real ->
      FFI.pokeByteOff ptr 0 RealFmt *> FFI.pokeByteOff ptr 8 real

    CStringVal str ->
      FFI.pokeByteOff ptr 0 (StringFmt @n) *> FFI.pokeByteOff ptr 8 str

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
    CVectorVal vec ->
      FFI.pokeByteOff ptr 0 (VectorFmt @n) *> FFI.pokeByteOff ptr 8 vec
#endif

    CTimeVal time ->
      FFI.pokeByteOff ptr 0 TimeFmt *> FFI.pokeByteOff ptr 8 time

    val ->
      IO.throwIO (InvalidValue val callStack)

data Value n where
  BitVal :: Bit -> Value 1
  BitVectorVal :: BitVector n -> Value n
  IntVal :: Int -> Value 32
#if defined(IVERILOG)
  RealVal :: Double -> Value 1
#else
  RealVal :: Double -> Value 64
#endif
  StringVal :: ByteString -> Value n
  TimeVal :: Time -> Value 64
  MiscVal :: ByteString -> Value n

deriving stock instance KnownNat n => Show (Value n)

instance (KnownNat n, 1 <= n) => UnsafeSend (Value n) where
  type Sent (Value n) = CValue n

  unsafeSend = \case
    BitVal bit ->
      CScalarVal <$> unsafeSend (bitToScalar bit)

    BitVectorVal bv ->
      let list = vectorToCVectorList (bitVectorToVector bv)
       in CVectorVal <$> unsafeSend list

    IntVal int ->
      pure (CIntVal (fromIntegral int))

    RealVal real ->
      pure (CRealVal (realToFrac real))

    StringVal str ->
      CStringVal <$> unsafeSend str

    TimeVal time -> do
      ctime <- unsafeSend @Time time
      ptr   <- fst <$> Sim.withNewPtr Sim.stackPtr (`FFI.poke` ctime)

      pure (CTimeVal ptr)

    MiscVal bytes ->
      CMiscVal <$> unsafeSend bytes

instance (KnownNat n, 1 <= n) => Send (Value n) where
  send = \case
    BitVal bit ->
      CScalarVal <$> send (bitToScalar bit)

    BitVectorVal bv ->
      let list = vectorToCVectorList (bitVectorToVector bv)
       in CVectorVal <$> send list

    IntVal int ->
      pure (CIntVal (fromIntegral int))

    RealVal real ->
      pure (CRealVal (realToFrac real))

    StringVal str ->
      CStringVal <$> send str

    TimeVal time -> do
      ctime <- send time
      ptr   <- fst <$> Sim.withNewPtr Sim.heapPtr (`FFI.poke` ctime)

      pure (CTimeVal ptr)

    MiscVal bytes ->
      CMiscVal <$> send bytes

instance (KnownNat n, 1 <= n) => UnsafeReceive (Value n) where
  type Received (Value n) = CValue n

  unsafeReceive = \case
    CBinStrVal _bin ->
      undefined -- TODO parser

    COctStrVal _oct ->
      undefined -- TODO parser

    CDecStrVal _dec ->
      undefined -- TODO parser

    CHexStrVal _hex ->
      undefined -- TODO parser

    CScalarVal scalar ->
      case sameNat (Proxy @1) (Proxy @n) of
        Just Refl -> BitVal . scalarToBit <$> unsafeReceive scalar
        Nothing   -> let size = natVal (Proxy @n)
                      in Sim.throw (InvalidSize 1 size callStack)

    CIntVal int ->
      case sameNat (Proxy @32) (Proxy @n) of
        Just Refl -> pure (IntVal (fromIntegral int))
        Nothing   -> let size = natVal (Proxy @n)
                      in Sim.throw (InvalidSize 32 size callStack)

    CRealVal real ->
#if defined(IVERILOG)
      case sameNat (Proxy @1) (Proxy @n) of
#else
      case sameNat (Proxy @64) (Proxy @n) of
#endif
        Just Refl -> pure (RealVal (realToFrac real))
        Nothing   -> let size = natVal (Proxy @n)
#if defined(IVERILOG)
                      in Sim.throw (InvalidSize 1 size callStack)
#else
                      in Sim.throw (InvalidSize 64 size callStack)
#endif

    CStringVal str ->
      StringVal <$> unsafeReceive str

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
    CVectorVal vec -> do
      BitVectorVal <$> unsafeReceive vec
#endif

    CTimeVal time ->
      case sameNat (Proxy @64) (Proxy @n) of
        Just Refl -> TimeVal <$> unsafePeekReceive time
        Nothing   -> let size = natVal (Proxy @n)
                      in Sim.throw (InvalidSize 64 size callStack)

    CMiscVal bytes ->
      MiscVal <$> unsafeReceive bytes

instance (KnownNat n, 1 <= n) => Receive (Value n) where
  receive = \case
    CBinStrVal _bin ->
      undefined -- TODO parser

    COctStrVal _oct ->
      undefined -- TODO parser

    CDecStrVal _dec ->
      undefined -- TODO parser

    CHexStrVal _hex ->
      undefined -- TODO parser

    CScalarVal scalar ->
      case sameNat (Proxy @1) (Proxy @n) of
        Just Refl -> BitVal . scalarToBit <$> receive scalar
        Nothing   -> let size = natVal (Proxy @n)
                      in Sim.throw (InvalidSize 1 size callStack)

    CIntVal int ->
      case sameNat (Proxy @32) (Proxy @n) of
        Just Refl -> pure (IntVal (fromIntegral int))
        Nothing   -> let size = natVal (Proxy @n)
                      in Sim.throw (InvalidSize 32 size callStack)

    CRealVal real ->
#if defined(IVERILOG)
      case sameNat (Proxy @1) (Proxy @n) of
#else
      case sameNat (Proxy @64) (Proxy @n) of
#endif
        Just Refl -> pure (RealVal (realToFrac real))
        Nothing   -> let size = natVal (Proxy @n)
#if defined(IVERILOG)
                      in Sim.throw (InvalidSize 1 size callStack)
#else
                      in Sim.throw (InvalidSize 64 size callStack)
#endif

    CStringVal str ->
      StringVal <$> receive str

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
    CVectorVal vec ->
      BitVectorVal <$> receive vec
#endif

    CTimeVal time ->
      case sameNat (Proxy @64) (Proxy @n) of
        Just Refl -> TimeVal <$> peekReceive time
        Nothing   -> let size = natVal (Proxy @n)
                      in Sim.throw (InvalidSize 64 size callStack)

    CMiscVal bytes ->
      MiscVal <$> receive bytes

data SomeValue where
  SomeValue :: KnownNat n => Value n -> SomeValue

instance Show SomeValue where
  show (SomeValue val) = show val

foreign import ccall "vpi_user.h vpi_get_value"
  c_vpi_get_value :: Handle -> Ptr (CValue n) -> IO ()

getValue
  :: (HasCallStack, KnownNat n, 1 <= n)
  => SimCont o (Ptr (CValue n))
  -> ValueFormat n
  -> Handle
  -> SimCont o (Ptr (CValue n))
getValue alloc fmt handle = do
  cfmt <- unsafeSend fmt

  fmap fst . Sim.withNewPtr alloc $ \ptr -> do
    FFI.pokeByteOff ptr 0 cfmt
    c_vpi_get_value handle ptr

    pure ()

unsafeReceiveValue
  :: (HasCallStack, KnownNat n, 1 <= n)
  => ValueFormat n
  -> Handle
  -> SimCont o (Value n)
unsafeReceiveValue fmt handle =
  getValue Sim.stackPtr fmt handle >>= unsafePeekReceive

receiveValue
  :: (HasCallStack, KnownNat n, 1 <= n)
  => ValueFormat n
  -> Handle
  -> SimCont o (Value n)
receiveValue fmt handle =
  getValue Sim.heapPtr fmt handle >>= peekReceive

foreign import ccall "vpi_user.h vpi_put_value"
  c_vpi_put_value :: Handle -> Ptr (CValue n) -> Ptr CTime -> CInt -> IO Handle

{-
NOTE [vpi_put_value and events]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In IEEE 1364, it mentions that the return value from vpi_put_value is a handle
to the event scheduled by the FFI call (i.e. an event to perform the value
change). This is returned when the vpiReturnEvent flag is set in the call,
otherwise it always returns NULL.

Currently, clash-ffi has no need to be able to cancel events before they are
executed. Instead of returning a handle from putValue, we silently discard the
result, and do not allow the high level API to set the vpiReturnEvent flag, so
a valid handle would never be returned anyway.
-}

unsafeSendValue
  :: (HasCallStack, KnownNat n, 1 <= n)
  => Handle
  -> Value n
  -> DelayMode
  -> SimCont o ()
unsafeSendValue handle value delay = do
  valuePtr <- unsafePokeSend value
  (timePtr, flags) <- unsafeSend delay

  Monad.void . IO.liftIO $
    c_vpi_put_value handle valuePtr timePtr flags

sendValue
  :: (HasCallStack, KnownNat n, 1 <= n)
  => Handle
  -> Value n
  -> DelayMode
  -> SimCont o ()
sendValue handle value delay = do
  valuePtr <- pokeSend value
  (timePtr, flags) <- send delay

  Monad.void . IO.liftIO $
    c_vpi_put_value handle valuePtr timePtr flags

