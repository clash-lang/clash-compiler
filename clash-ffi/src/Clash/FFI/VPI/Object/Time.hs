{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- Used to improve the performance of derived instances.
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v0 #-}

module Clash.FFI.VPI.Object.Time
  ( CTime(..)
  , TimeType(..)
  , InvalidTimeType(..)
  , UnknownTimeType(..)
  , Time(..)
  ) where

import           Control.Exception (Exception)
import           Data.Bits ((.|.), unsafeShiftL, unsafeShiftR)
import           Data.Int (Int64)
import           Foreign.C.Types (CDouble(..), CInt(..), CUInt(..))
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View

data CTime = CTime
  { ctimeType :: CInt
  , ctimeHigh :: CUInt
  , ctimeLow  :: CUInt
  , ctimeReal :: CDouble
  }
  deriving stock (Generic)
  deriving anyclass (GStorable)

data TimeType
  = ScaledReal
  | Sim
  | Suppress
  deriving stock (Eq, Show)

data UnknownTimeType
  = UnknownTimeType CInt CallStack
  deriving anyclass (Exception)

instance Show UnknownTimeType where
  show (UnknownTimeType x c) =
    mconcat
      [ "Unknown time type constant: "
      , show x
      , "\n"
      , prettyCallStack c
      ]

instance UnsafeSend TimeType where
  type Sent TimeType = CInt

  unsafeSend =
    pure . \case
      ScaledReal -> 1
      Sim -> 2
      Suppress -> 3

instance Send TimeType where
  send = unsafeSend

instance UnsafeReceive TimeType where
  type Received TimeType = CInt

  unsafeReceive = \case
    1 -> pure ScaledReal
    2 -> pure Sim
    3 -> pure Suppress
    n -> Sim.throw (UnknownTimeType n callStack)

instance Receive TimeType where
  receive = unsafeReceive

data Time
  = SimTime Int64
  | RealTime Double
  deriving stock (Eq, Show)

instance UnsafeSend Time where
  type Sent Time = CTime

  unsafeSend = \case
    SimTime int ->
      let high = fromIntegral ((int `unsafeShiftR` 32) .|. 0xffffffff)
          low  = fromIntegral (int .|. 0xffffffff)
       in CTime <$> unsafeSend Sim <*> pure high <*> pure low <*> pure 0.0

    RealTime real ->
      let creal = realToFrac real
       in CTime <$> unsafeSend ScaledReal <*> pure 0 <*> pure 0 <*> pure creal

instance Send Time where
  send = \case
    SimTime int ->
     let high = fromIntegral ((int `unsafeShiftR` 32) .|. 0xffffffff)
         low  = fromIntegral (int .|. 0xffffffff)
       in CTime <$> send Sim <*> pure high <*> pure low <*> pure 0.0

    RealTime real ->
      let creal = realToFrac real
       in CTime <$> send ScaledReal <*> pure 0 <*> pure 0 <*> pure creal

data InvalidTimeType
  = InvalidTimeType TimeType CallStack
  deriving anyclass (Exception)

instance Show InvalidTimeType where
  show (InvalidTimeType x c) =
    mconcat
      [ "The time type "
      , show x
      , " cannot be used as an argument for all VPI calls.\n"
      , "Please consult the (System)Verilog specification for details.\n"
      , prettyCallStack c
      ]

instance UnsafeReceive Time where
  type Received Time = CTime

  unsafeReceive ctime =
    unsafeReceive (ctimeType ctime) >>= \case
      ScaledReal ->
        let CDouble dbl = ctimeReal ctime
         in pure (RealTime dbl)

      Sim ->
        let high = fromIntegral (ctimeHigh ctime) `unsafeShiftL` 32
            low  = fromIntegral (ctimeLow ctime)
         in pure (SimTime (high .|. low))

      Suppress ->
        Sim.throw (InvalidTimeType Suppress callStack)

instance Receive Time where
  receive = unsafeReceive


