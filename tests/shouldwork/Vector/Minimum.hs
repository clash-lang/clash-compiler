module Minimum where

import Clash.Prelude

topEntity :: Vec 3 Int -> Int
topEntity = minimum
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure (4 :> 8 :> (-2) :> Nil)
    expectedOutput = outputVerifier (singleton (-2))
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
