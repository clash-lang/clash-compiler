module SFixedTest where

import Clash.Prelude

type SF = SFixed 4 18

topEntity :: SF -> SF
topEntity x = x * 2.56
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = stimuliGenerator $(listToVecTH ([1.2, 1.8, 3.5] :: [SFixed 4 18] ))
    expectedOutput = outputVerifier   $(listToVecTH ([3.07199, 4.607991, 8.96] :: [SFixed 4 18]))
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
