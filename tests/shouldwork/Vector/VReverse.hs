module VReverse where

import Clash.Prelude

topEntity :: Vec 4 Int -> Vec 4 Int
topEntity = reverse
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure (iterateI (+1) 1)
    expectedOutput = outputVerifier ((4:>3:>2:>1:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
