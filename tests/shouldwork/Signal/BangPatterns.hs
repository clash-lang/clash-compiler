{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module BangPatterns where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Signal System (BitVector 1)
topEntity = f @System (pure False)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

f :: Signal dom Bool -> Signal dom (BitVector 1)
f !e = fmap pack e

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst (0 :> Nil)
    done           = expectedOutput topEntity
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
