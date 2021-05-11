{-# LANGUAGE TypeApplications #-}

module T1796 where

import Clash.Prelude

import Clash.Explicit.Testbench

topEntity
  :: Signal System Bool
  -> Signal System Bool
topEntity = id
{-# NOINLINE topEntity #-}

tb
  :: Signal System Bool
tb = done
  where
    testInput = stimuliGenerator clk rst (singleton True)
    expectOutput = outputVerifier' clk rst (singleton True)
    done = expectOutput $ topEntity testInput
    clk = tbClockGen @System (not <$> done)
    rst = resetGen @System
{-# INLINE tb #-}

testBench :: Signal System Bool
testBench = tb
{-# NOINLINE testBench #-}
