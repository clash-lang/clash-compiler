{-# LANGUAGE CPP #-}

module ZipWithUnitSP where

import Clash.Prelude
import Clash.Explicit.Testbench

data PQR = P () | Q | R () deriving (Eq, Show, Generic, ShowX)

topEntity
  :: Vec 2 PQR
  -> Vec 2 (Index 2, PQR)
topEntity xs = zipWith (,) indicesI xs
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (repeat (P ()) :> repeat Q :> Nil)
    expectedOutput = outputVerifier' clk rst (((0, P ()) :> (1, P ()) :> Nil)
                                          :> ((0, Q)    :> (1, Q)    :> Nil)
                                          :> Nil)

    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
