-- Regression test: a zero-width half of @split#@ must stay foldable, so that a
-- constant case subject built from it folds away instead of leaving a
-- redundant multiplexer in the generated HDL.
--
-- This test passes @-Werror=clash-unmatchable-constant@, which turns the
-- warning @caseCon@ emits for such a subject into an error. See
-- @tests/Main.hs@.
{-# LANGUAGE MagicHash #-}

module SplitZeroWidth where

import Clash.Prelude
import Clash.Explicit.Testbench

import Clash.Sized.Internal.BitVector (split#, (++#))
import qualified Clash.Sized.Internal.BitVector as BV

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
