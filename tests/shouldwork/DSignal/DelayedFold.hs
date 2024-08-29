module DelayedFold where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D
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
  => DSignal dom 0 (Vec 5 Int)
  -> DSignal dom 3 Int
folder = zeroAt0 . delayedFold d1 0 (*) . D.unbundle

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Vec 5 Int)
  -> Signal System Int
topEntity = exposeClockResetEnable (toSignal . folder . fromSignal)

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $ repeat 1 :> repeat 2 :> (1:>2:>3:>4:>5:>Nil) :> Nil
    expectedOutput = outputVerifier' clk rst (0:>0:>1:>1:>32:>120:>Nil)
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
