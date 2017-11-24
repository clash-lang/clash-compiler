module Resize where

import Clash.Prelude

topEntity :: Signed 4 -> Signed 3
topEntity = resize
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = stimuliGenerator $(listToVecTH ([minBound .. maxBound]::[Signed 4]))
    expectedOutput = outputVerifier $(listToVecTH ([-4,-3,-2,-1,-4,-3,-2,-1,0,1,2,3,0,1,2,3]::[Signed 3]))
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
