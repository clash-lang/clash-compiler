{-# LANGUAGE CPP #-}

module T2149 where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Word -> Signed 8
topEntity = fromIntegral
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (negNr 42 :> posNr 41 :> negNr (-40) :> posNr (-39) :> Nil)
    expectedOutput = outputVerifier' clk rst (42 :> 41 :> (-40) :> (-39) :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen


-- | Use input as the lower byte of the output, and set its bits 31 and 63
--
-- By setting both bit 31 and 63, the sign-bit of the intermediate Integer is always set,
-- no matter if we're representing it as a signed 64 or 32 bit number.
negNr :: Signed 8 -> Word
negNr x = unpack (resize (pack x)) .|. bit 31 .|. bit 63


posNr :: Signed 8 -> Word
posNr x = unpack (resize (pack x)) .|. bit 30
