{-# LANGUAGE CPP #-}

module ClassOps where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: (Integer,Integer) -> Integer
topEntity = uncurry mod
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $(listToVecTH [(19,4)::(Integer,Integer),(7,3),(55,-10),(9,-2),(0,-10),(11,10)])
    expectedOutput = outputVerifier' clk rst $(listToVecTH ([3::Integer,1,-5,-1,0,1]))
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
