module ZeroInt where

import Clash.Prelude

topEntity :: (UFixed 0 8,UFixed 0 8) -> UFixed 0 8
topEntity = uncurry (*)
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure (0.2,0.35)
    expectedOutput = outputVerifier $(listToVecTH [0.06640625 :: UFixed 0 8])
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
