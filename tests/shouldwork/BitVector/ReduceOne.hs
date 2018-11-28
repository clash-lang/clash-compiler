module ReduceOne where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock  System Source
  -> Reset  System Asynchronous
  -> Signal System (Signed 1)
  -> Signal System (Bit, Bit, Bit)
topEntity clk rst =
  fmap (\a -> (reduceAnd a, reduceOr a, reduceXor a))
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((1 :: Signed 1) :> 0 :> Nil)
    expectedOutput = outputVerifier clk rst ((high, high, high) :> (low, low, low) :> Nil)
    done           = expectedOutput (topEntity clk rst testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
