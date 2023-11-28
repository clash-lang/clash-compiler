{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

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

import           Control.Exception (Exception, throwIO)
import           Data.Bits ((.|.), (.&.), unsafeShiftL, unsafeShiftR)
import           Data.Int (Int64)
import           Foreign.C.Types (CDouble(..), CInt(..), CUInt(..))
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import           Clash.FFI.View

-- | The low level representation of a VPI time value, as sent and received by
-- VPI calls. This can optionally be converted to a 'Time' using 'Receive'.
--
-- For the high-level representation of time values that can be obtained using
-- 'Receive', see 'Time'.
--
data CTime = CTime
  { ctimeType :: CInt
  , ctimeHigh :: CUInt
  , ctimeLow  :: CUInt
  , ctimeReal :: CDouble
  }
  deriving stock (Generic)
  deriving anyclass (GStorable)

-- | The type of a time value, used for API calls where only the format of time
-- is needed instead of a specific time value. There are two formats of time:
--
--   * scaled real time, which gives time in seconds as a double
--   * simulation time, which gives the time in units of some precision
--
-- Additionally, there is the special @SuppressTime@ format, which specifies no
-- time value will be given by the simulator. This is used for callbacks when
-- the time is not needed in the callback routine.
--
data TimeType
  = ScaledReal
  | Sim
  | SuppressTime
  deriving stock (Eq, Show)

-- | An exception thrown when decoding a time format if an invalid value is
-- given for the C enum that specifies the constructor of 'TimeType'.
--
data UnknownTimeType
  = UnknownTimeType CInt CallStack
  deriving anyclass (Exception)

instance Show UnknownTimeType where
  show = \case
    UnknownTimeType x c -> mconcat
      [ "Unknown time type constant: "
      , show x
      , "\n"
      , prettyCallStack c
      ]

type instance CRepr TimeType = CInt

instance Send TimeType where
  send =
    pure . \case
      ScaledReal -> 1
      Sim -> 2
      SuppressTime -> 3

instance Receive TimeType where
  receive = \case
    1 -> pure ScaledReal
    2 -> pure Sim
    3 -> pure SuppressTime
    n -> throwIO $ UnknownTimeType n callStack

-- | A value of time, used as either the current point in time or a duration
-- depending on the context. This represents time as either the number of units
-- of the time precision (@SimTime@), or the amount of seconds the simulation
-- has been running for (@RealTime@).
--
-- For the low-level representation of time values that are sent / received by
-- VPI calls, see 'CTime'.
--
data Time
  = SimTime Int64
  | RealTime Double
  deriving stock (Eq, Show)

type instance CRepr Time = CTime

instance Send Time where
  send = \case
    SimTime int ->
     let high = fromIntegral ((int `unsafeShiftR` 32) .&. 0xffffffff)
         low  = fromIntegral (int .&. 0xffffffff)
       in CTime <$> send Sim <*> pure high <*> pure low <*> pure 0.0

    RealTime real ->
      let creal = realToFrac real
       in CTime <$> send ScaledReal <*> pure 0 <*> pure 0 <*> pure creal

-- | An exception thrown when requesting time in a format that is not supported
-- by an operation. Typically this occurs from attempting to suppress time
-- where it is not allowed.
--
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

instance Receive Time where
  receive ctime =
    receive (ctimeType ctime) >>= \case
      ScaledReal ->
        let CDouble dbl = ctimeReal ctime
         in pure (RealTime dbl)

      Sim ->
        let high = fromIntegral (ctimeHigh ctime) `unsafeShiftL` 32
            low  = fromIntegral (ctimeLow ctime)
         in pure $ SimTime (high .|. low)

      SuppressTime ->
        throwIO $ InvalidTimeType SuppressTime callStack
