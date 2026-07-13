{-# LANGUAGE NoImplicitPrelude #-}

module T3308b where

import Clash.Prelude
import Clash.Explicit.Testbench

-- Same issue (#3308) in the `splitAt` reduction: it inlined the recursive
-- `splitAt` (and hence its vector argument) into both the `fst` and `snd`
-- projections, duplicating a co-recursive `scanl` spine exponentially.
topEntity :: Unsigned 8
topEntity = last (fst (splitAt (SNat @20) spine))
 where
  spine = scanl const (1 :: Unsigned 8) (repeat 0 :: Vec 39 (Unsigned 8)) :: Vec 40 (Unsigned 8)
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  done = outputVerifier' clk rst (1 :> Nil) (pure topEntity)
  clk  = tbSystemClockGen (not <$> done)
  rst  = systemResetGen
