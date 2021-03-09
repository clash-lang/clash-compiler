module DelayI where

import Clash.Prelude
import Clash.Explicit.Testbench

zeroAt0
  :: HiddenClockResetEnable dom
  => DSignal dom n Int
  -> DSignal dom n Int
zeroAt0 a = unsafeFromSignal (mux en (toSignal a) 0)
  where
    en = register False (pure True)

delayer
  :: HiddenClockResetEnable dom
  => DSignal dom 0 Int
  -> DSignal dom 2 Int
delayer = zeroAt0 . delayI 0

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Int
  -> Signal System Int
topEntity = exposeClockResetEnable (toSignal . delayer . fromSignal)

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $ 1 :> 2 :> 3 :> 10 :> Nil
    expectedOutput = outputVerifier' clk rst (0:>1:>1:>2:>3:>10:>Nil)
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
