module Concat where

import CLaSH.Prelude

topEntity :: Vec 2 (Vec 3 (Unsigned 8)) -> Vec 6 (Unsigned 8)
topEntity = concat

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure ((1 :> 2 :> 3 :> Nil) :> (4 :> 5 :> 6 :> Nil) :> Nil)
    expectedOutput = outputVerifier ((1:>2:>3:>4:>5:>6:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (systemClock (not <$> done')) systemReset done
