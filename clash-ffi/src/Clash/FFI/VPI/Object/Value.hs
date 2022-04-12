{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Object.Value
  ( CValue(..)
  , Value(..)

  , module Clash.FFI.VPI.Object.Value.Delay
  , module Clash.FFI.VPI.Object.Value.Format
  , module Clash.FFI.VPI.Object.Value.Scalar
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
  , module Clash.FFI.VPI.Object.Value.Vector
#endif
  ) where

import qualified Control.Exception as IO (throwIO)
import           Data.ByteString (ByteString)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CDouble, CInt(..))
import           Foreign.Ptr (Ptr)
import           Foreign.Storable as FFI (Storable(..))
import           GHC.Stack (callStack)
import           GHC.TypeNats (SomeNat(..), someNatVal)

import           Clash.Promoted.Nat (SNat(..), snatProxy)
import           Clash.Sized.BitVector (Bit, BitVector)
import           Clash.Sized.Signed (Signed)

import qualified Clash.FFI.Monad as Sim (heapPtr, stackPtr, withNewPtr)
import           Clash.FFI.View
import           Clash.FFI.VPI.Object.Time
import           Clash.FFI.VPI.Object.Value.Delay
import           Clash.FFI.VPI.Object.Value.Format
import           Clash.FFI.VPI.Object.Value.Parse
import           Clash.FFI.VPI.Object.Value.Scalar

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
import           Clash.FFI.VPI.Object.Value.Vector
#endif

-- | The low level representation of a VPI value, as sent and received by VPI
-- calls. This can optionally be converted to a 'Value' using 'Receive'.
--
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
  deriving stock (Show)

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

-- | A value is a Clash-compatible representation of a given VPI value. This
-- represents values with the corresponding type from @clash-prelude@ where
-- possible, or the higher-level representation from this library (in the case
-- of things like time values).
--
-- For the low-level representation of values that is sent / received by VPI
-- calls, see 'CValue'.
--
data Value where
  BitVal       :: Bit -> Value
  BitVectorVal :: SNat n -> BitVector n -> Value
  IntVal       :: Signed 32 -> Value
  RealVal      :: Double -> Value
  StringVal    :: SNat n -> ByteString -> Value
  TimeVal      :: Time -> Value

instance Show Value where
  show = \case
    BitVal bit -> show bit
    BitVectorVal SNat bv -> show bv
    IntVal int -> show int
    RealVal real -> show real
    StringVal _ str -> show str
    TimeVal time -> show time

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

instance Send Value where
  send = \case
    BitVal bit ->
      CScalarVal <$> send (bitToScalar bit)

    BitVectorVal SNat bv ->
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
      CVectorVal <$> send bv
#else
      error "Send.Value: BitVector without VPI_VECVAL"
#endif

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

instance UnsafeReceive Value where
  type Received Value = (CValue, CInt)

  unsafeReceive (cvalue, size) =
    case cvalue of
      CBinStrVal bin ->
        case someNatVal (fromIntegral size) of
          SomeNat proxy ->
            BitVectorVal (snatProxy proxy) <$> parseBinStr size bin

      COctStrVal oct ->
        case someNatVal (fromIntegral size) of
          SomeNat proxy ->
            BitVectorVal (snatProxy proxy) <$> parseOctStr size oct

      CDecStrVal dec -> do
        case someNatVal (fromIntegral size) of
          SomeNat proxy ->
            BitVectorVal (snatProxy proxy) <$> parseDecStr size dec

      CHexStrVal hex ->
        case someNatVal (fromIntegral size) of
          SomeNat proxy ->
            BitVectorVal (snatProxy proxy) <$> parseHexStr size hex

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

instance Receive Value where
  receive (cvalue, size) =
    case cvalue of
      CBinStrVal bin ->
        case someNatVal (fromIntegral size) of
          SomeNat proxy ->
            BitVectorVal (snatProxy proxy) <$> parseBinStr size bin

      COctStrVal oct ->
        case someNatVal (fromIntegral size) of
          SomeNat proxy ->
            BitVectorVal (snatProxy proxy) <$> parseOctStr size oct

      CDecStrVal dec -> do
        case someNatVal (fromIntegral size) of
          SomeNat proxy ->
            BitVectorVal (snatProxy proxy) <$> parseDecStr size dec

      CHexStrVal hex ->
        case someNatVal (fromIntegral size) of
          SomeNat proxy ->
            BitVectorVal (snatProxy proxy) <$> parseHexStr size hex

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

