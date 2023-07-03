{-# LANGUAGE CPP #-}

module Unconcat where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Vec 6 (Unsigned 8) -> Vec 2 (Vec 3 (Unsigned 8))
topEntity = unconcatI
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (1 :> 2 :> 3 :> 4 :> 5 :> 6 :> Nil)
    expectedOutput = outputVerifier' clk rst (((1 :> 2 :> 3 :> Nil) :> (4 :> 5 :> 6 :> Nil) :> Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
