{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

module MonomorphicTopEntity where

import Clash.Prelude
import Clash.Explicit.Testbench

data STy ty where
  SBool  :: Bool -> STy Bool
  SInt16 :: Signed 16 -> STy (Signed 16)

sty :: STy ty -> ty
sty (SBool b) = not b
sty (SInt16 i) = i + 1
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE sty #-}

topEntity :: STy (Signed 16) -> Signed 16
topEntity = sty
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (SInt16 0 :> SInt16 1 :> SInt16 2 :> SInt16 3 :> Nil)
    expectedOutput = outputVerifier' clk rst (1 :> 2 :> 3 :> 4 :> Nil)

    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
