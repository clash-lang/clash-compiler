module DelayN where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D
import Clash.Explicit.Testbench

delayer
  :: HiddenClockReset domain gated synchronous
  => DSignal domain 0 Int
  -> DSignal domain 2 Int
delayer = delayN d2 0

topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Signal System Int
  -> Signal System Int
topEntity = exposeClockReset (toSignal . delayer . fromSignal)

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $ 1 :> 2 :> 3 :> 10 :> Nil
    expectedOutput = outputVerifier clk rst (0:>1:>1:>2:>3:>10:>Nil)
    done           = expectedOutput (topEntity clk rst testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen

