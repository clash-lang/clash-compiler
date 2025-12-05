{-# LANGUAGE CPP #-}

module HeadM where

import Clash.Prelude
import Clash.Explicit.Testbench

head' :: Vec 3 (Signed 16) -> Signed 16
head' (Cons x xs) = x
{-# OPAQUE head' #-}

topEntity :: Vec 3 (Signed 16) -> Signed 16
topEntity = head'
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((1 :> 2 :> 3 :> Nil) :> Nil)
    expectedOutput = outputVerifier' clk rst (1 :> Nil)

    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
