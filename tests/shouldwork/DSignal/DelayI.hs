module DelayI where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D
import Clash.Explicit.Testbench

delayer
  :: HiddenClockResetEnable dom conf
  => DSignal dom 0 Int
  -> DSignal dom 2 Int
delayer = delayI 0

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
    expectedOutput = outputVerifier clk rst (0:>1:>1:>2:>3:>10:>Nil)
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
