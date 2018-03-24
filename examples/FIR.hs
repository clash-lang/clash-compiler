module FIR where

import Clash.Prelude
import Clash.Explicit.Testbench

dotp :: SaturatingNum a
     => Vec (n + 1) a
     -> Vec (n + 1) a
     -> a
dotp as bs = fold boundedPlus (zipWith boundedMult as bs)

fir
  :: (Default a, KnownNat n, SaturatingNum a, HiddenClockReset domain)
  => Vec (n + 1) a -> Signal domain a -> Signal domain a
fir coeffs x_t = y_t
  where
    y_t = dotp coeffs <$> bundle xs
    xs  = window x_t

topEntity
  :: Clock  System Source
  -> Reset  System Asynchronous
  -> Signal System (Signed 16)
  -> Signal System (Signed 16)
topEntity = exposeClockReset (fir (2:>3:>(-2):>8:>Nil))
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (2:>3:>(-2):>8:>Nil)
    expectedOutput = outputVerifier clk rst (4:>12:>1:>20:>Nil)
    done           = expectedOutput (topEntity clk rst testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
