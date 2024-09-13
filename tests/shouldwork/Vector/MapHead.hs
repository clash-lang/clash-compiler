-- see issue #2809
module MapHead where

import Clash.Prelude
import Clash.Explicit.Testbench


topEntity :: Vec 2 (Vec 2 Int) -> Vec 2 Int
topEntity = map head
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk aclr (((0 :> 1 :> Nil) :> (3 :> 4 :> Nil) :> Nil) :> Nil)
    expectedOutput = outputVerifier'  clk aclr ((0 :> 3 :> Nil) :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    aclr           = systemResetGen
