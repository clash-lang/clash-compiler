-- Regression test for the @(++#)@ evaluator rule when a zero-width operand is
-- not a literal.
--
-- @split#@ with a zero-width half is reduced by @reduceSplitHandler@, which
-- replaces that half by @Clash.Normalize.Primitives.removedArg@: a constant,
-- but not a literal. When such a value ends up in a constant case subject
-- (here through @fromEnum#@), the evaluator must still reduce the @(++#)@.
-- It used to get stuck on the non-literal operand, which made @caseCon@ report
-- "Unmatchable constant as case subject" and left a redundant multiplexer in
-- the generated HDL instead of folding the case away.
--
-- This test passes @-fclash-werror@, which turns that warning into an error.
-- See @tests/Main.hs@.
{-# LANGUAGE MagicHash #-}

module AppendRemovedArg where

import Clash.Prelude
import Clash.Explicit.Testbench

import Clash.Sized.Internal.BitVector (split#, (++#))
import qualified Clash.Sized.Internal.BitVector as BV

-- | Normalizes to @removedArg \@(BitVector 0)@, no matter what @bv@ is.
zeroWidth :: BitVector 8 -> BitVector 0
zeroWidth bv = fst (split# bv :: (BitVector 0, BitVector 8))

topEntity :: BitVector 8 -> Unsigned 8
topEntity bv =
  case BV.fromEnum# (zeroWidth bv ++# (0xF0 :: BitVector 8)) of
    240 -> 1
    _ -> 2
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput = stimuliGenerator clk rst (0 :> 0xAB :> Nil)
  expectedOutput = outputVerifier' clk rst (1 :> 1 :> Nil)
  done = expectedOutput (topEntity <$> testInput)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
{-# OPAQUE testBench #-}
