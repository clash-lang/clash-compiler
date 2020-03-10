module T1187 where

-- See https://github.com/clash-lang/clash-compiler/issues/1187
-- I couldn't reduce the test case any further :(

import T1187.Keypad (inputKeypad)
import T1187.SerialTx (serialTx)
import Clash.Prelude

type Digit = Index 10

topEntity
    :: Clock System
    -> Signal System Bit
    -> Signal System (Vec 4 Bool)
    -> ( Signal System Bit
      , Signal System (Vec 4 Bool)
      )
topEntity clk = withClockResetEnable clk resetGen enableGen board
  where
    board rx rows = (tx, cols)
      where
        digits = logic @4 cmd

        (tx, ack) = serialTx @8 (SNat @9600) (fmap bitCoerce <$> serialDisplay ack digits)
        cmd = (const Nothing =<<) <$> (serialRx @8 (SNat @9600) rx)

        (cols, _) = inputKeypad (repeat $ repeat (0 :: Int))  rows

serialDisplay
    :: forall n dom. (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Vec n (Maybe Digit))
    -> Signal dom (Maybe (Unsigned 8))
serialDisplay _ack _digits = pure Nothing

logic
    :: forall n dom. (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom (Maybe ())
    -> Signal dom (Vec n (Maybe Digit))
logic = const $ pure $ repeat Nothing

serialRx :: forall n rate dom. SNat rate -> Signal dom Bit -> Signal dom (Maybe (Vec n Bit))
serialRx _rate = undefined

