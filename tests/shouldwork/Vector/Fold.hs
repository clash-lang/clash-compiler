module Fold where

import Clash.Prelude

topEntity :: Vec 8 Int -> Int
topEntity = fold (+)
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure (1:>2:>3:>4:>5:>6:>7:>8:>Nil)
    expectedOutput = outputVerifier (36 :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
