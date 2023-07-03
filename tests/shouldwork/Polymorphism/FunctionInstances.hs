{-# LANGUAGE CPP #-}

module FunctionInstances where

import Clash.Prelude
import Clash.Explicit.Testbench

class Bus p where
 type MasterToSlave p

instance Bus (a -> b) where
 type MasterToSlave (a -> b) = a

f :: (Eq a, Num a) => a -> (MasterToSlave (a -> a))
f x =
  case x + 1 of
    2 -> 3
    _ -> 5
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE f #-}

topEntity :: Signal System Int -> Signal System Int
topEntity = fmap f
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput    = stimuliGenerator clk rst (0 :> 1 :> 2 :> Nil)
    expectOutput = outputVerifier' clk rst (5 :> 3 :> 5 :> Nil)
    done         = expectOutput (topEntity testInput)
    clk          = tbSystemClockGen (not <$> done)
    rst          = systemResetGen
