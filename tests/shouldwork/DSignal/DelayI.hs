module DelayI where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D
import Clash.Explicit.Testbench

delayer :: ( HiddenClockReset domain gated synchronous )
        => DSignal domain 0 Int -> DSignal domain 2 Int
delayer = delayI

zeroFirst2
  :: (Num a, HiddenClockReset domain gated synchronous )
  => Signal domain a -> Signal domain a
zeroFirst2 a = mux en a 0
  where
    en = register False $ register False $ pure True

topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Signal System Int
  -> Signal System Int
topEntity = exposeClockReset (zeroFirst2 . toSignal . delayer . fromSignal)

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $ 1 :> 2 :> 3 :> 10 :> Nil
    expectedOutput = outputVerifier clk rst (0:>0:>1:>2:>3:>10:>Nil)
    done           = expectedOutput (topEntity clk rst testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
