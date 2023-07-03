{-# LANGUAGE CPP #-}

module Resize where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Signed 4 -> Signed 3
topEntity = resize
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $(listToVecTH ([minBound .. maxBound]::[Signed 4]))
    expectedOutput = outputVerifier' clk rst $(listToVecTH ([-4,-3,-2,-1,-4,-3,-2,-1,0,1,2,3,0,1,2,3]::[Signed 3]))
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
