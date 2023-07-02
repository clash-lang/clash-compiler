{-# LANGUAGE CPP #-}

module T2040 where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Signal System (Unsigned 8)
  -> Signal System (Unsigned 8)
topEntity = id
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput = register clk rst en 0 (testInput + 1)
  expectedOutput =
    outputVerifier' clk rst $(listToVecTH [0 :: Unsigned 8 .. 3])
  done = expectedOutput $ topEntity testInput
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
  en = enableGen
