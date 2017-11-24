module VMerge where

import Clash.Prelude

topEntity :: (Vec 2 Int,Vec 2 Int) -> Vec 4 Int
topEntity (x,y) = merge x y
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure (iterateI (+1) 1,iterateI (+1) 3)
    expectedOutput = outputVerifier ((1:>3:>2:>4:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
