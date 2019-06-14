module FoldrEmpty where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: SystemClockResetEnable
  => Signal System Int
  -> Signal System Int
topEntity x =
  delayBy (SNat @0) x
{-# NOINLINE topEntity #-}

delayBy n =
  foldr (.) id (replicate n id)
{-# NOINLINE delayBy #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (2 :> 3 :> 4 :> 5 :> Nil)
    expectedOutput = outputVerifier clk rst (2 :> 3 :> 4 :> 5 :> Nil)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
    done =
      expectedOutput
        (withClockResetEnable clk rst enableGen topEntity testInput)
