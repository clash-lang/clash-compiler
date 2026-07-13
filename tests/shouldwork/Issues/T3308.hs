{-# LANGUAGE NoImplicitPrelude #-}

module T3308 where

import Clash.Prelude
import Clash.Explicit.Testbench

-- #3308: the evaluator's `zipWith` reduction (reached here via `foldl`) inlined
-- its vector argument into both the head and the recursive tail, duplicating a
-- co-recursively defined vector spine exponentially in the vector length.
topEntity :: Unsigned 8
topEntity = snd (foldl const (0 :: Unsigned 8, 1) (repeat 0 :: Vec 20 (Unsigned 8)))
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  done = outputVerifier' clk rst (1 :> Nil) (pure topEntity)
  clk  = tbSystemClockGen (not <$> done)
  rst  = systemResetGen
