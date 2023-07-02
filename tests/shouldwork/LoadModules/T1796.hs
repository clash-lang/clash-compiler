{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module T1796 where

import Clash.Prelude

import Clash.Explicit.Testbench

topEntity
  :: Signal System Bool
  -> Signal System Bool
topEntity = id
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

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
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
