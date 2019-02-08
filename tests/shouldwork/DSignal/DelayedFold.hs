module DelayedFold where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D
import Clash.Explicit.Testbench

folder
  :: HiddenClockReset domain gated synchronous
  => DSignal domain 0 (Vec 4 Int)
  -> DSignal domain 2 Int
folder = delayedFold d1 0 (+) . D.unbundle

topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Signal System (Vec 4 Int)
  -> Signal System Int
topEntity = exposeClockReset (toSignal . folder . fromSignal)

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $ repeat 1 :> repeat 2 :> (1:>2:>3:>4:>Nil) :> Nil
    expectedOutput = outputVerifier clk rst (0:>4:>4:>8:>10:>Nil)
    done           = expectedOutput (topEntity clk rst testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen

