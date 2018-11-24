module DelayedFoled where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D
import Clash.Explicit.Testbench

folder :: ( HiddenClockReset domain gated synchronous )
      => DSignal domain 0 (Vec 4 Int) -> DSignal domain 2 Int
folder = delayedFold d1 (+) . D.unbundle

topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Signal System (Vec 4 Int)
  -> Signal System (Maybe Int)
topEntity = exposeClockReset (fmap maybeX . toSignal . folder . fromSignal)

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $ repeat 1 :> repeat 2 :> (1:>2:>3:>4:>Nil) :> Nil
    expectedOutput = outputVerifier clk rst (Nothing :> Nothing :>Just 4:>Just 8:>Just 10:>Nil)
    done           = expectedOutput (topEntity clk rst testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen

