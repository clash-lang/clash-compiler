{-# LANGUAGE CPP #-}

module T1322 where

import Clash.Prelude
import Clash.Explicit.Testbench

incr :: Index 3 -> Index 3
incr i = if i == maxBound then 0 else i + 1
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE incr #-}

topEntity :: Index 10 -> Index 3
topEntity j = case j < 1 of
  False ->
    let xs :: Vec 1 (Index 3)
        xs = init (1 :> (incr (head ys) :> Nil))
        {-# NOINLINE xs #-}
        ys :: Vec 1 (Index 3)
        ys = init (1 :> (incr (head xs) :> Nil))
        {-# NOINLINE ys #-}
    in incr (incr (head xs))
  True ->
    let ys :: Vec 1 (Index 3)
        ys = init (2 :> (incr (head xs) :> Nil))
        {-# NOINLINE xs #-}
        xs :: Vec 1 (Index 3)
        xs = init (2 :> (incr (head ys) :> Nil))
        {-# NOINLINE ys #-}
    in incr (incr (head ys))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $(listToVecTH [0 :: (Index 10),1])
    expectedOutput = outputVerifier' clk rst $(listToVecTH [1 :: Index 3,0])
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
