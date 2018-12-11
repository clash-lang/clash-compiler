-- Make sure a gated clock doesn't influence the values around it in a product type
--
-- The Netlist code used to think a gated clock was 1 bit wide.
-- This caused the generated Verilog to shift the other bit of the clock into res1.
module GatedClockWidth where

import Clash.Explicit.Prelude

topEntity
  :: Clock System Source
  -> Signal System (Unsigned 8)
  -> (Signal System (Unsigned 8), Clock System Gated, Signal System (Unsigned 8))
topEntity clk x = clkGater clk x
{-# NOINLINE topEntity #-}

clkGater :: Clock domain Source -> Signal domain (Unsigned 8) -> (Signal domain (Unsigned 8), Clock domain Gated, Signal domain (Unsigned 8))
clkGater clk x = (succ <$> x, clkOut, x)
  where
    clkOut = clockGate clk ((== 3) <$> x)
{-# NOINLINE clkGater #-}

testBench :: Signal System Bool
testBench = done
  where
    testValues     = $(listToVecTH ([1..10]::[Unsigned 8]))
    testInput      = stimuliGenerator clk rst testValues
    expectedOutput = outputVerifier clk rst $ zip (succ <$> testValues) testValues
    (res1,_,res2)  = topEntity clk testInput
    done           = expectedOutput $ bundle (res1,res2)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
