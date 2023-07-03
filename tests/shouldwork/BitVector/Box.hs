{-# LANGUAGE CPP #-}

module Box where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: BitVector 16 -> (BitVector 16,BitVector 8,Vec 8 Bit)
topEntity vec = (pack  tup
                ,pack  ((0 :: Bit) :> 0 :> 0 :> 0 :> 1 :> 1 :> 1 :> 1 :> Nil)
                ,unpack 0xF0
                )
  where
    tup :: (Vec 8 Bit, Vec 8 Bit)
    tup = unpack vec
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (0x00FF)
    expectedOutput = outputVerifier' clk rst ((0x00FF,0x0F,1:>1:>1:>1:>0:>0:>0:>0:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
