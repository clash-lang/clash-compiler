{-# LANGUAGE CPP #-}

module VFold where

import Clash.Prelude
import Clash.Explicit.Testbench

csSort = vfold (const csRow)
  where
    cs a b     = if a > b then (a,b) else (b,a)
    csRow y xs = let (y',xs') = mapAccumL cs y xs in xs' :< y'

topEntity :: Vec 4 Int -> Vec 4 Int
topEntity = csSort
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (7 :> 3 :> 9 :> 1 :> Nil)
    expectedOutput = outputVerifier' clk rst ((1:>3:>7:>9:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
