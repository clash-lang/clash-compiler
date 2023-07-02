{-# LANGUAGE CPP #-}

module Fib where

import Clash.Prelude
import Clash.Explicit.Testbench

fib :: HiddenClockResetEnable dom => Signal dom (Unsigned 64)
fib = register 1 fib + register 0 (register 0 fib)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Unsigned 64)
topEntity = exposeClockResetEnable fib
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst $(listToVecTH [1 :: Unsigned 64,1,2,3,5])
    done           = expectedOutput (topEntity clk rst enableGen)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
