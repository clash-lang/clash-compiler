module FindIndex where

import Clash.Prelude

topEntity :: Vec 7 (Unsigned 8) -> Maybe (Index 7)
topEntity = findIndex (> 3)
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure (1:>3:>2:>4:>3:>5:>6:>Nil)
    expectedOutput = outputVerifier ((Just 3) :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
