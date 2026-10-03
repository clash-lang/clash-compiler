{-# LANGUAGE NoImplicitPrelude #-}

module IterateSharing where

import qualified Prelude as P

import Clash.Prelude
import Clash.Explicit.Testbench

-- The evaluator used to substitute the fields of a scrutinized constructor
-- into the chosen alternative. 'go' uses 'a' twice: the comparison evaluated
-- one copy, while the result held on to an unevaluated copy. Iterating 'go'
-- then built (Fibonacci-sized) terms that grew exponentially with the vector
-- length.
topEntity :: Vec 26 (Unsigned 8)
topEntity = takeI (map fst (iterateI @27 go (0, 1)))
 where
  go (a, b) = if a == maxBound then (a, b) else (a + b, a)
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  expected =
    $(listToVecTH
        (P.take 26 (P.map P.fst (P.iterate
          (\(a, b) -> if a == maxBound then (a, b) else (a + b, a))
          (0 :: Unsigned 8, 1)))))
  done = outputVerifier' clk rst (expected :> Nil) (pure topEntity)
  clk  = tbSystemClockGen (not <$> done)
  rst  = systemResetGen
