module Unconcat where

import Clash.Prelude

topEntity :: Vec 6 (Unsigned 8) -> Vec 2 (Vec 3 (Unsigned 8))
topEntity = unconcatI
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure (1 :> 2 :> 3 :> 4 :> 5 :> 6 :> Nil)
    expectedOutput = outputVerifier (((1 :> 2 :> 3 :> Nil) :> (4 :> 5 :> 6 :> Nil) :> Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
