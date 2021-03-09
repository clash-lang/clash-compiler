module DelayedFold where

import Clash.Prelude
import Clash.Explicit.Testbench

zeroAt0
  :: HiddenClockResetEnable dom
  => DSignal dom n Int
  -> DSignal dom n Int
zeroAt0 a = unsafeFromSignal (mux en (toSignal a) 0)
  where
    en = register False (pure True)

folder
  :: HiddenClockResetEnable dom
  => DSignal dom 0 (Vec 4 Int)
  -> DSignal dom 2 Int
folder = zeroAt0 . delayedFold d1 0 (+) . unbundle
--{-# NOINLINE folder #-}

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Vec 4 Int)
  -> Signal System Int
topEntity = exposeClockResetEnable (toSignal . folder . fromSignal)

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $ repeat 1 :> repeat 2 :> (1:>2:>3:>4:>Nil) :> Nil
    expectedOutput = outputVerifier' clk rst (0:>4:>4:>8:>10:>Nil)
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen

