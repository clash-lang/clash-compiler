{-# LANGUAGE CPP #-}

module ClassOps where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: (Int,Int) -> Int
topEntity = uncurry mod
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $(listToVecTH [(19,4)::(Int,Int),(7,3),(55,-10),(9,-2),(0,-10),(11,10)])
    expectedOutput = outputVerifier' clk rst $(listToVecTH ([3::Int,1,-5,-1,0,1]))
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
