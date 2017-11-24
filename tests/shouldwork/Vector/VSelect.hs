module VSelect where

import Clash.Prelude

topEntity :: Vec 8 Int -> Vec 4 Int
topEntity x = select d1 d2 d4 x
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure (iterateI (+1) 1)
    expectedOutput = outputVerifier ((2:>4:>6:>8:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
