module Fib where

import Clash.Prelude
import Clash.Explicit.Testbench

fib :: HiddenClockReset dom => Signal dom (Unsigned 64)
fib = register 1 fib + register 0 (register 0 fib)

topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Signal System (Unsigned 64)
topEntity = exposeClockReset fib
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier clk rst $(listToVecTH [1 :: Unsigned 64,1,2,3,5])
    done           = expectedOutput (topEntity clk rst)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
