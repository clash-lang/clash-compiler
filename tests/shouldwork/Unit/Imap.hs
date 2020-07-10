-- See: https://github.com/clash-lang/clash-compiler/issues/507

module Imap where

import Clash.Prelude
import Clash.Explicit.Testbench

data AB = A | B deriving (Eq, Generic, ShowX)

ab :: KnownNat n => SatIndex 'SatError n -> AB -> AB
ab n A = if n >  0 then A else B
ab n B = if n == 0 then B else A
{-# NOINLINE ab #-}

topEntity :: Vec 1 AB -> Vec 1 AB
topEntity = imap ab
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((A :> Nil) :> (B :> Nil) :> Nil)
    expectedOutput = outputVerifier' clk rst ((B :> Nil) :> (B :> Nil) :> Nil)

    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
