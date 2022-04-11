{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Value
  ( CValue(..)
  , Value(..)
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
import           Data.Coerce
import           Data.Typeable (Typeable)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CDouble, CInt(..))
import           Foreign.Ptr (Ptr)
import           Foreign.Storable as FFI (Storable(..))
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import           GHC.TypeNats

import           Clash.Promoted.Nat (SNat(..), snatProxy)
import           Clash.Sized.BitVector (Bit, BitVector)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (heapPtr, stackPtr, withNewPtr)
import           Clash.FFI.View
import           Clash.FFI.VPI.Object
import           Clash.FFI.VPI.Value.Delay
import           Clash.FFI.VPI.Value.Format
import           Clash.FFI.VPI.Value.Scalar

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
import           Clash.FFI.VPI.Value.Vector
#endif

data CValue
  = CBinStrVal CString
  | COctStrVal CString
  | CDecStrVal CString
  | CHexStrVal CString
  | CScalarVal CInt
  | CIntVal CInt
  | CRealVal CDouble
  | CStringVal CString
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
  | CVectorVal (Ptr CVector)
#endif
  | CTimeVal (Ptr CTime)
  | CMiscVal CString
  deriving stock (Show)

data InvalidFormat
  = InvalidFormat ValueFormat CallStack
  deriving anyclass (Exception)

instance Show InvalidFormat where
  show (InvalidFormat f c) =
    mconcat
      [ "The value format "
      , show f
      , " can not be used in all calls.\n"
      , "Please consult the (System)Verilog specification for details.\n"
      , prettyCallStack c
      ]

data InvalidValue
  = InvalidValue CValue CallStack
  deriving anyclass (Exception)

instance Show InvalidValue where
  show (InvalidValue v c) =
    mconcat
      [ "Attempt to send/receive a value "
      , show v
      , " which has a format with no data.\n"
      , prettyCallStack c
      ]

instance Storable CValue where
  sizeOf _ = 16
  alignment _ = 8

  peek ptr =
    FFI.peekByteOff @ValueFormat ptr 0 >>= \case
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
      FFI.pokeByteOff ptr 0 BinStrFmt *> FFI.pokeByteOff ptr 8 bin

    COctStrVal oct ->
      FFI.pokeByteOff ptr 0 OctStrFmt *> FFI.pokeByteOff ptr 8 oct

    CDecStrVal dec ->
      FFI.pokeByteOff ptr 0 DecStrFmt *> FFI.pokeByteOff ptr 8 dec

    CHexStrVal hex ->
      FFI.pokeByteOff ptr 0 HexStrFmt *> FFI.pokeByteOff ptr 8 hex

    CScalarVal scalar ->
      FFI.pokeByteOff ptr 0 ScalarFmt *> FFI.pokeByteOff ptr 8 scalar

    CIntVal int ->
      FFI.pokeByteOff ptr 0 IntFmt *> FFI.pokeByteOff ptr 8 int

    CRealVal real ->
      FFI.pokeByteOff ptr 0 RealFmt *> FFI.pokeByteOff ptr 8 real

    CStringVal str ->
      FFI.pokeByteOff ptr 0 StringFmt *> FFI.pokeByteOff ptr 8 str

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
    CVectorVal vec ->
      FFI.pokeByteOff ptr 0 VectorFmt *> FFI.pokeByteOff ptr 8 vec
#endif

    CTimeVal time ->
      FFI.pokeByteOff ptr 0 TimeFmt *> FFI.pokeByteOff ptr 8 time

    value ->
      IO.throwIO (InvalidValue value callStack)

data Value where
  BitVal       :: Bit -> Value
  BitVectorVal :: SNat n -> BitVector n -> Value
  IntVal       :: Int -> Value
  RealVal      :: Double -> Value
  StringVal    :: SNat n -> ByteString -> Value
  TimeVal      :: Time -> Value
  MiscVal      :: SNat n -> ByteString -> Value

instance Show Value where
  show = \case
    BitVal bit -> show bit
    BitVectorVal SNat bv -> show bv
    IntVal int -> show int
    RealVal real -> show real
    StringVal _ str -> show str
    TimeVal time -> show time
    MiscVal _ bytes -> show bytes

instance UnsafeSend Value where
  type Sent Value = CValue

  unsafeSend = \case
    BitVal bit ->
      CScalarVal <$> unsafeSend (bitToScalar bit)

    BitVectorVal SNat bv ->
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
      CVectorVal <$> unsafeSend bv
#else
      error "UnsafeSend.Value: BitVector without VPI_VECVAL"
#endif

    IntVal int ->
      pure (CIntVal (fromIntegral int))

    RealVal real ->
      pure (CRealVal (realToFrac real))

    StringVal _size str ->
      CStringVal <$> unsafeSend str

    TimeVal time -> do
      ctime <- unsafeSend @Time time
      ptr <- fst <$> Sim.withNewPtr Sim.stackPtr (`FFI.poke` ctime)

      pure (CTimeVal ptr)

    MiscVal _size bytes ->
      CMiscVal <$> unsafeSend bytes

instance Send Value where
  send = \case
    BitVal bit ->
      CScalarVal <$> send (bitToScalar bit)

    BitVectorVal SNat bv ->
      CVectorVal <$> send bv

    IntVal int ->
      pure (CIntVal (fromIntegral int))

    RealVal real ->
      pure (CRealVal (realToFrac real))

    StringVal _size str ->
      CStringVal <$> send str

    TimeVal time -> do
      ctime <- send time
      ptr <- fst <$> Sim.withNewPtr Sim.heapPtr (`FFI.poke` ctime)

      pure (CTimeVal ptr)

    MiscVal _size bytes ->
      CMiscVal <$> send bytes

instance UnsafeReceive Value where
  type Received Value = (CValue, CInt)

  unsafeReceive (cvalue, size) =
    case cvalue of
      CBinStrVal _bin ->
        undefined -- TODO parser

      COctStrVal _oct ->
        undefined -- TODO parser

      CDecStrVal _dec ->
        undefined -- TODO parser

      CHexStrVal _hex ->
        undefined -- TODO parser

      CScalarVal scalar ->
        BitVal . scalarToBit <$> unsafeReceive scalar

      CIntVal int ->
        pure (IntVal (fromIntegral int))

      CRealVal real ->
        pure (RealVal (realToFrac real))

      CStringVal str -> do
        case someNatVal (fromIntegral size) of
          SomeNat proxy -> StringVal (snatProxy proxy) <$> unsafeReceive str

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
      CVectorVal vec ->
        case someNatVal (fromIntegral size) of
          SomeNat proxy -> BitVectorVal (snatProxy proxy) <$> unsafeReceive vec
#endif

      CTimeVal time ->
        TimeVal <$> unsafePeekReceive time

      CMiscVal bytes -> do
        case someNatVal (fromIntegral size) of
          SomeNat proxy -> MiscVal (snatProxy proxy) <$> unsafeReceive bytes

instance Receive Value where
  receive (cvalue, size) =
    case cvalue of
      CBinStrVal _bin ->
        undefined -- TODO parser

      COctStrVal _oct ->
        undefined -- TODO parser

      CDecStrVal _dec ->
        undefined -- TODO parser

      CHexStrVal _hex ->
        undefined -- TODO parser

      CScalarVal scalar ->
        BitVal . scalarToBit <$> receive scalar

      CIntVal int ->
        pure (IntVal (fromIntegral int))

      CRealVal real ->
        pure (RealVal (realToFrac real))

      CStringVal str -> do
        case someNatVal (fromIntegral size) of
          SomeNat proxy -> StringVal (snatProxy proxy) <$> receive str

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
      CVectorVal vec ->
        case someNatVal (fromIntegral size) of
          SomeNat proxy -> BitVectorVal (snatProxy proxy) <$> receive vec
#endif

      CTimeVal time ->
        TimeVal <$> peekReceive time

      CMiscVal bytes -> do
        case someNatVal (fromIntegral size) of
          SomeNat proxy -> MiscVal (snatProxy proxy) <$> receive bytes

foreign import ccall "vpi_user.h vpi_get_value"
  c_vpi_get_value :: Object -> Ptr CValue -> IO ()

getValue
  :: HasCallStack
  => Coercible handle Object
  => SimCont o (Ptr CValue)
  -> ValueFormat
  -> handle
  -> SimCont o (Ptr CValue)
getValue alloc fmt handle = do
  cfmt <- unsafeSend fmt

  fmap fst . Sim.withNewPtr alloc $ \ptr -> do
    FFI.pokeByteOff ptr 0 cfmt
    c_vpi_get_value (coerce handle) ptr

    pure ()

unsafeReceiveValue
  :: forall handle o
   . HasCallStack
  => Coercible handle Object
  => Show handle
  => Typeable handle
  => ValueFormat
  -> handle
  -> SimCont o Value
unsafeReceiveValue fmt handle = do
  ptr <- getValue Sim.stackPtr fmt (coerce @handle @Object handle)
  cvalue <- IO.liftIO (FFI.peek ptr)
  size <- getProperty Size handle

  unsafeReceive (cvalue, size)

receiveValue
  :: forall handle o
   . HasCallStack
  => Coercible handle Object
  => Show handle
  => Typeable handle
  => ValueFormat
  -> handle
  -> SimCont o Value
receiveValue fmt handle = do
  ptr <- getValue Sim.heapPtr fmt (coerce @handle @Object handle)
  cvalue <- IO.liftIO (FFI.peek ptr)
  size <- getProperty Size handle

  receive (cvalue, size)

foreign import ccall "vpi_user.h vpi_put_value"
  c_vpi_put_value :: Object -> Ptr CValue -> Ptr CTime -> CInt -> IO Object

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
  :: HasCallStack
  => Coercible handle Object
  => handle
  -> Value
  -> DelayMode
  -> SimCont o ()
unsafeSendValue handle value delay = do
  valuePtr <- unsafePokeSend value
  (timePtr, flags) <- unsafeSend delay

  Monad.void . IO.liftIO $
    c_vpi_put_value (coerce handle) valuePtr timePtr flags

sendValue
  :: HasCallStack
  => Coercible handle Object
  => handle
  -> Value
  -> DelayMode
  -> SimCont o ()
sendValue handle value delay = do
  valuePtr <- pokeSend value
  (timePtr, flags) <- send delay

  Monad.void . IO.liftIO $
    c_vpi_put_value (coerce handle) valuePtr timePtr flags

