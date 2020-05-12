module T1322 where

import Clash.Prelude
import Clash.Explicit.Testbench

incr :: Index 3 -> Index 3
incr i = if i == maxBound then 0 else i + 1
{-# NOINLINE incr #-}

topEntity :: Index 10 -> Index 3
topEntity j = case j < 1 of
  False ->
    let xs :: Vec 1 (Index 3)
        xs = init (Cons 1 (Cons (incr (head ys)) Nil))
        {-# NOINLINE xs #-}
        ys :: Vec 1 (Index 3)
        ys = init (Cons 1 (Cons (incr (head xs)) Nil))
        {-# NOINLINE ys #-}
    in incr (incr (head xs))
  True ->
    let ys :: Vec 1 (Index 3)
        ys = init (Cons 2 (Cons (incr (head xs)) Nil))
        {-# NOINLINE xs #-}
        xs :: Vec 1 (Index 3)
        xs = init (Cons 2 (Cons (incr (head ys)) Nil))
        {-# NOINLINE ys #-}
    in incr (incr (head ys))
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $(listToVecTH [0 :: (Index 10),1])
    expectedOutput = outputVerifier' clk rst $(listToVecTH [1 :: Index 3,0])
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
