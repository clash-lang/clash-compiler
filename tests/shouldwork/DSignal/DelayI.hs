module DelayI where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D
import Clash.Explicit.Testbench

delayer :: ( HiddenClockReset domain gated synchronous )
        => DSignal domain 0 Int -> DSignal domain 2 Int
delayer = delayI

topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Signal System Int
  -> Signal System (Maybe Int)
topEntity = exposeClockReset (fmap maybeX . toSignal . delayer . fromSignal)

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $ 1 :> 2 :> 3 :> 10 :> Nil
    expectedOutput = outputVerifier clk rst (Nothing:>Nothing:>Just 1:>Just 2:>Just 3:>Just 10:>Nil)
    done           = expectedOutput (topEntity clk rst testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen

