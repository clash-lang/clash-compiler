module DeepseqX where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: (Vec 2 Int, Vec 3 Int) -> Vec 3 Int
topEntity (a, b) = deepseqX a (map (+1) b)


testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((10 :> 11 :> Nil, 5 :> 6 :> 7 :> Nil) :> Nil)
    expectedOutput = outputVerifier   clk rst ((6 :> 7 :> 8 :> Nil) :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
