
-- See: https://github.com/clash-lang/clash-compiler/issues/365
{-# LANGUAGE CPP #-}

module Replace where

import Clash.Prelude
import Clash.Explicit.Testbench

import Data.Word

topEntity
  :: SystemClockResetEnable
  => Signal System Word8
topEntity = fmap head r
  where
    r = register (0 :> Nil) (fmap f r)
      where
        f :: Vec 1 Word8 -> Vec 1 Word8
        f regs = replace 0 (regs!!0 + 1) regs
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst (0 :> 1 :> 2 :> 3 :> Nil)
    done           = expectedOutput (exposeClockResetEnable topEntity clk rst enableGen)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
