{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
module ByteSwap32 where

import Clash.Prelude
import Clash.Explicit.Testbench
import GHC.Word
import Data.Bits

topEntity :: Word32 -> Word32
topEntity = byteSwap32
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    unswapped      =       0x01 :>       0x03 :>       0x08 :>       0x32 :> 0 :> 0x12345678 :> Nil
    swapped        = 0x01000000 :> 0x03000000 :> 0x08000000 :> 0x32000000 :> 0 :> 0x78563412 :> Nil
    testInput      = stimuliGenerator clk rst $ unswapped ++ swapped
    expectedOutput = outputVerifier' clk rst $ swapped ++ unswapped
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
