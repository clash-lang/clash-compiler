{-# LANGUAGE CPP #-}

module ReduceZero where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock  System
  -> Reset  System
  -> Enable System
  -> Signal System (Signed 0)
  -> Signal System (Bit, Bit, Bit)
topEntity clk rst en =
  fmap (\a -> (reduceAnd a, reduceOr a, reduceXor a))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((0 :: Signed 0) :> Nil)
    expectedOutput = outputVerifier' clk rst ((high, low, low) :> Nil)
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
