{-# LANGUAGE CPP #-}

module FIR where

import Clash.Prelude
import Clash.Explicit.Testbench

dotp :: SaturatingNum a
     => Vec (n + 1) a
     -> Vec (n + 1) a
     -> a
dotp as bs = fold boundedAdd (zipWith boundedMul as bs)

fir
  :: ( HiddenClockResetEnable tag
     , Default a
     , KnownNat n
     , SaturatingNum a
     , NFDataX a )
  => Vec (n + 1) a -> Signal tag a -> Signal tag a
fir coeffs x_t = y_t
  where
    y_t = dotp coeffs <$> bundle xs
    xs  = window x_t

topEntity
  :: Clock  System
  -> Reset  System
  -> Enable System
  -> Signal System (Signed 16)
  -> Signal System (Signed 16)
topEntity = exposeClockResetEnable (fir (2:>3:>(-2):>8:>Nil))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (2:>3:>(-2):>8:>Nil)
    expectedOutput = outputVerifier' clk rst (4:>12:>1:>20:>Nil)
    done           = expectedOutput (topEntity clk rst (enableGen) testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
