{-# LANGUAGE CPP #-}

module Mixer where

import Clash.Prelude
import Clash.Explicit.Testbench

k               = 0.6
piFHalf         = 1.5707963267948966 :: SFixed 3 8

cordic angle
    | z < 0     =  k
    | otherwise = -k
    where
        z       | angle < 0 = piFHalf + angle
                | otherwise = (-piFHalf)+ angle


topEntity :: SFixed 3 8 -> SFixed 3 8
topEntity = cordic
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (0.7853981633974483 :> Nil)
    expectedOutput = outputVerifier'   clk rst (0.59765625 :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
