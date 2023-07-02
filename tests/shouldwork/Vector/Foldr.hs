{-# LANGUAGE CPP #-}

module Foldr where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Vec 4 (Unsigned 8) -> (Unsigned 8)
topEntity = foldr div 1
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (24 :> 7 :> 4 :> 2 :> Nil)
    expectedOutput = outputVerifier' clk rst (8 :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
