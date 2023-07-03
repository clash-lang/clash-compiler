{-# LANGUAGE CPP #-}

module VEmpty where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: SystemClockResetEnable
  => Signal System Int
  -> Signal System Int
topEntity x =
  delayBy (SNat @2) x
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

delayBy n signal =
  foldr (\_ s -> s + 10) signal (replicate n ())
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE delayBy #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (2 :> 3 :> 4 :> 5 :> Nil)
    expectedOutput = outputVerifier' clk rst (22 :> 23 :> 24 :> 25 :> Nil)
    done           = expectedOutput (withClockResetEnable clk rst enableGen topEntity testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
