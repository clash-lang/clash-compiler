module T3291 where

import Clash.Prelude
import Clash.Explicit.Testbench
import T3291.BitPackC

packUSym ::
  forall n. (KnownNat n) =>
  Unsigned n -> Vec (ByteSizeC (Unsigned n)) (BitVector 8)
packUSym = packC
{-# OPAQUE packUSym #-}

topEntity :: () -> Vec (ByteSizeC (Unsigned 9)) (BitVector 8)
topEntity () = packUSym @9 3
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput = stimuliGenerator clk rst (() :> Nil)
  expectedOutput =
    outputVerifier' clk rst
      ((0b0000_0011 :> 0b0000_0000 :> Nil) :> Nil)
  done = expectedOutput (topEntity <$> testInput)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
{-# OPAQUE testBench #-}
