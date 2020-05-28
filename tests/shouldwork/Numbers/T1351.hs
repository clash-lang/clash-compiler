module T1351 where

import Clash.Prelude
import Clash.Explicit.Testbench

type F = Signed 8

topEntity :: HiddenClockResetEnable System => Signal System F -> Signal System F
topEntity i = boundedAdd <$> 0 <*> (last $ generate d1 (register 0) i)
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (-4:>Nil)
    expectedOutput = outputVerifier' clk rst (0:>(-4):>Nil)
    done           = expectedOutput (exposeClockResetEnable topEntity clk rst (enableGen) testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
