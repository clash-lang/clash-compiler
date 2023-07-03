{-# LANGUAGE CPP #-}

module VIndicesI where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Signal System (Vec 4 (Index 4))
topEntity = pure indicesI
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst ((0:>1:>2:>3:>Nil):>Nil)
    done           = expectedOutput topEntity
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
