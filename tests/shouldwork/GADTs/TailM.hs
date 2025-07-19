{-# LANGUAGE CPP #-}

module TailM where

import Clash.Prelude
import Clash.Explicit.Testbench

tail' :: Vec 3 (Signed 16) -> Vec 2 (Signed 16)
tail' (x :> xs) = xs
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE tail' #-}

topEntity :: Vec 3 (Signed 16) -> Vec 2 (Signed 16)
topEntity = tail'
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((1 :> 2 :> 3 :> Nil) :> Nil)
    expectedOutput = outputVerifier' clk rst ((2 :> 3 :> Nil) :> Nil)

    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
