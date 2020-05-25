module TRepeat2 where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity x = register @System (trepeat @2 True) x
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst input
    expectedOutput = outputVerifier' clk rst expected
    done           = expectedOutput $ withClockResetEnable clk rst en topEntity testInput
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
    en             = enableGen

input = replicate d2 $([| v2t (replicate d4 False) |])
expected = $([| v2t (replicate d4 $ True) :> v2t (replicate d4 (False))  :> Nil |])
