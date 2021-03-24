module T1715 where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: (BitVector 16,BitVector 128) -> BitVector 128
topEntity = uncurry (setSlice d127 d112)
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((maxBound,0):>Nil)
    expectedOutput = outputVerifier' clk rst (340277174624079928635746076935438991360:>Nil)
    done           = expectedOutput (fmap topEntity testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
