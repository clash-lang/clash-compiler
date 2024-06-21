{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
module T1187.SerialTx
    ( serialTx
    , serialTxDyn
    , fifo
    ) where

import Clash.Prelude
import T1187.Utils (mealyStateB)

import Control.Monad.State

import Data.Word


txStep :: Word32 -> Maybe (Vec n Bit) -> State Int (Bit, Bool)
txStep _bitDuration _input = undefined

serialTxDyn
    :: (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Word32
    -> Signal dom (Maybe (Vec n Bit))
    -> (Signal dom Bit, Signal dom Bool)
serialTxDyn bitDuration input = mealyStateB (uncurry txStep) 0 (bitDuration, input)

serialTx
    :: forall n rate dom. (KnownNat n, KnownNat (ClockDivider dom (HzToPeriod rate)), HiddenClockResetEnable dom)
    => SNat rate
    -> Signal dom (Maybe (Vec n Bit))
    -> (Signal dom Bit, Signal dom Bool)
serialTx _rate = serialTxDyn $ pure . fromIntegral . natVal $ SNat @(ClockDivider dom (HzToPeriod rate))

fifo
    :: forall a dom. (NFDataX a, HiddenClockResetEnable dom)
    => Signal dom (Maybe a) -> Signal dom Bool -> Signal dom (Maybe a)
fifo _input _outReady = undefined
