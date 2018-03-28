{-# LANGUAGE MagicHash #-}
module ByteSwap32 where

import Clash.Prelude
import Clash.Explicit.Testbench
import GHC.Word
import Data.Bits

topEntity :: Word32 -> Word32
topEntity = byteSwap32
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $(listToVecTH [1::Word32,3,8,50,0])
    expectedOutput = outputVerifier clk rst $(listToVecTH ([16777216,50331648,134217728,838860800,0]::[Word32]))
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
