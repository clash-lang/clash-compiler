-- Regression test: 'popCount', 'countLeadingZeros' and 'countTrailingZeros' on
-- the Clash number types must not drag 'Integer' into the netlist.
--
-- They used to be defined in terms of @fromInteger . Index.toInteger#@, which
-- becomes @GHC.Num.Integer.integerToInt#@ in the netlist. That primitive
-- carries a "dubious primitive instantiation" warning, and rightly so:
-- 'Integer' is unbounded in simulation but fixed-width after synthesis. They
-- now go through @Index.fromEnum#@, which has a black box of its own.
--
-- This test passes @-fclash-werror@, which turns any Clash warning -- including
-- the dubious-primitive one -- into an error. See @tests/Main.hs@.
module PopCountNoInteger where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity ::
  (BitVector 9, Unsigned 9, Signed 9, Index 300) ->
  Vec 12 Int
topEntity (bv, u, s, i) =
     popCount bv :> countLeadingZeros bv :> countTrailingZeros bv
  :> popCount u  :> countLeadingZeros u  :> countTrailingZeros u
  :> popCount s  :> countLeadingZeros s  :> countTrailingZeros s
  :> popCount i  :> countLeadingZeros i  :> countTrailingZeros i
  :> Nil
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  -- 0b0_0011_0000 in 9 bits: 2 bits set, 3 leading zeros, 4 trailing zeros.
  -- 'Index 300' is 9 bits wide too (CLog 2 300 == 9), so 48 gives the same.
  testInput = stimuliGenerator clk rst ((0x030, 0x030, 0x030, 48) :> Nil)
  expectedOutput =
    outputVerifier' clk rst
      ((  2 :> 3 :> 4
       :> 2 :> 3 :> 4
       :> 2 :> 3 :> 4
       :> 2 :> 3 :> 4
       :> Nil) :> Nil)
  done = expectedOutput (topEntity <$> testInput)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
{-# OPAQUE testBench #-}
