module VRotate where

import Clash.Prelude

topEntity :: Vec 5 Int -> (Vec 5 Int,Vec 5 Int)
topEntity v = (rotateLeftS v d2,rotateRightS v d2)
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure (1:>2:>3:>4:>5:>Nil)
    expectedOutput = outputVerifier ((3:>4:>5:>1:>2:>Nil,4:>5:>1:>2:>3:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
