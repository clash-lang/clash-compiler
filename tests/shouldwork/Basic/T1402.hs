{-# OPTIONS_GHC -fno-worker-wrapper #-}
module T1402 where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Maybe (BitVector 128) -> BitVector 128 -> (BitVector 128,Bool)
topEntity q y = case q of
  Just b -> f True b
  _      -> f False y

f :: Bool -> BitVector 128 ->  (BitVector 128, Bool)
f t x = case x of
  1 -> (x,t)
  _ -> (x,t)

testBench :: Signal System Bool
testBench = done
  where
    testInput1 = stimuliGenerator clk rst $(listToVecTH
        [Just (maxBound :: (BitVector 128))])
    testInput2 = stimuliGenerator clk rst $(listToVecTH
        [maxBound :: (BitVector 128)])
    expectedOutput = outputVerifier' clk rst $(listToVecTH
        [(maxBound :: (BitVector 128), True)])
    done           = expectedOutput (topEntity <$> testInput1 <*> testInput2)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
