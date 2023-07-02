{-# LANGUAGE CPP #-}

module ZipWithUnitSP2 where

import Clash.Prelude
import Clash.Explicit.Testbench

data YZA = Y Int | Z () | A deriving (Eq, Show, Generic, ShowX)

topEntity
  :: Vec 2 YZA
  -> Vec 2 (Index 2, YZA)
topEntity xs = zipWith (,) indicesI xs
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (repeat (Y 2) :> repeat (Z ()) :> repeat A :> Nil)
    expectedOutput = outputVerifier' clk rst (((0, Y 2) :> (1, Y 2) :> Nil)
                                          :> ((0, Z ()):> (1, Z ()):> Nil)
                                          :> ((0, A)   :> (1, A)   :> Nil)
                                          :> Nil)

    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
