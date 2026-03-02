{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module CountTrailingZeros where

#include "MachDeps.h"

import Clash.Explicit.Testbench
import Clash.Prelude
import Data.Bits
import GHC.Word

topEntity :: Word -> Int
topEntity = countTrailingZeros
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput = stimuliGenerator clk rst $ (listToVecTH [1 :: Word, 3, 8, 50, 0])
#if WORD_SIZE_IN_BITS == 64
    expectedOutput = outputVerifier' clk rst $(listToVecTH ([0,0,3,1,64]::[Int]))
#elif WORD_SIZE_IN_BITS == 32
    expectedOutput = outputVerifier' clk rst $(listToVecTH ([0,0,3,1,32]::[Int]))
#else
#error Unsupported word size
#endif
    done = expectedOutput (topEntity <$> testInput)
    clk = tbSystemClockGen (not <$> done)
    rst = systemResetGen
