-- see https://github.com/clash-lang/clash-compiler/issues/383
{-# LANGUAGE CPP #-}

module FirOddSize where

import Clash.Prelude
import Clash.Explicit.Testbench

fir coeffs x = dotp coeffs (window x)
  where
    dotp as bs = sum (zipWith (*) as bs)

topEntity
  :: Clock  System
  -> Reset  System
  -> Enable System
  -> Signal System (Signed 16)
  -> Signal System (Signed 16)
topEntity = exposeClockResetEnable (fir (2:>3:>(-2):>8:>0:>Nil))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (2:>3:>(-2):>8:>1:>Nil)
    expectedOutput = outputVerifier' clk rst (4:>12:>1:>20:>54:>Nil)
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
