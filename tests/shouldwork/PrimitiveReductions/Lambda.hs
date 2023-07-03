{-# LANGUAGE CPP #-}

module Lambda where

-- Test if Clash will try to reduce "myConstant" to a constant even though it
-- is marked NOINLINE and even though it is a "complex" expression. Register
-- needs myConstant to be constant.

import Clash.Prelude
import Clash.Explicit.Testbench

-- TODO: Figure out why compilation time scales super-linearly upon increasing
-- TODO: VecSize.
type VecSize = 32

g :: Bool -> Vec (n + 1) Int -> Vec (n + 1) Int
g b xs = (+ 1) (head xs) :> tail xs
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE g #-}

myConstant :: Bool -> Vec VecSize Int
myConstant b = g b (map (+1) (repeat 3))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE myConstant #-}

topEntity
  :: SystemClockResetEnable
  => Signal System (Vec VecSize Int)
  -> Signal System (Vec VecSize Int)
topEntity = register (myConstant True)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (repeat 8 :> repeat 9 :> Nil)
    expectedOutput = outputVerifier' clk rst ((5 :> repeat 4) :> repeat 8 :> repeat 9 :> Nil)
    done           = expectedOutput (exposeClockResetEnable topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
