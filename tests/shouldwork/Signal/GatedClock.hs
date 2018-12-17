module GatedClock where

import Clash.Explicit.Prelude
import Clash.Annotations.SynthesisAttributes

test
  :: Clock System gated
  -> Reset System Asynchronous
  -> Signal System (Unsigned 8)
  -> Signal System (Unsigned 8)
test clkIn rst x = register clk rst 0 x
  where
    clk = clockGate clkIn clkEn
    clkEn = even <$> x


{-# ANN gated (defSyn "gated") #-}
gated = test @Gated
{-# NOINLINE gated #-}

{-# ANN source (defSyn "source") #-}
source = test @Source
{-# NOINLINE source #-}



{-# ANN testBench (defSyn "testbench") #-}
testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clkS rst $(listToVecTH ([1..10] :: [Unsigned 8]))
    expectedOutput = outputVerifier clkS rst $(listToVecTH ([0,0,2,2,4,4,6,6,8,8,10,10] :: [Unsigned 8]))
    testOutput1    = source clkS  rst testInput
    testOutput2    = gated  clkG rst testInput
    done1          = expectedOutput testOutput1
    done2          = expectedOutput testOutput2
    done           = done1 .&&. done2
    clkS           = tbSystemClockGen (not <$> done)
    clkG           = clockGate clkS (pure True)
    rst            = systemResetGen
