module ClassOps where

import Clash.Prelude

topEntity :: (Integer,Integer) -> Integer
topEntity = uncurry mod
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = stimuliGenerator $(listToVecTH [(19,4)::(Integer,Integer),(7,3),(55,-10),(9,-2),(0,-10),(11,10)])
    expectedOutput = outputVerifier $(listToVecTH ([3::Integer,1,-5,-1,0,1]))
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
