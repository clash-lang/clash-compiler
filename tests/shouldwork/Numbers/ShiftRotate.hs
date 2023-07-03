{-
This tests the HDL implementations of shiftL,shiftR,rotateL and rotateR
by checking their results against compile-time evaluated calls to the same functions.

So it checks that the HDL implementations have the same behavior as the Haskell implementations.
(And it assumes that the Haskell implementations are correct.)
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module ShiftRotate where
import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Int
import Data.Word
import ShiftRotateBase

topEntity = testall
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

expected :: Vec _ ( Vec Ops (Unsigned 8)
                  , Vec Ops (Signed 8)
                  , Vec Ops (BitVector 8)
                  , Vec Ops Word8
                  , Vec Ops Int8
                  , Vec Ops (Unsigned 70)
                  )
expected = $(lift $ map (testall testpattern) amounts)

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst amounts
    expectedOutput = outputVerifier' clk rst expected
    done           = expectedOutput ((liftA2 topEntity) (pure testpattern) testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
