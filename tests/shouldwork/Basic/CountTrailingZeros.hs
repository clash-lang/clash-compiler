{-# LANGUAGE CPP, MagicHash #-}
module CountTrailingZeros where

#include "MachDeps.h"

import Clash.Prelude
import GHC.Word
import Data.Bits

topEntity :: Word -> Int
topEntity = countTrailingZeros
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = stimuliGenerator $(listToVecTH [1::Word,3,8,50,0])
#if WORD_SIZE_IN_BITS == 64
    expectedOutput = outputVerifier $(listToVecTH ([0,0,3,1,64]::[Int]))
#elif WORD_SIZE_IN_BITS == 32
    expectedOutput = outputVerifier $(listToVecTH ([0,0,3,1,32]::[Int]))
#else
#error Unsupported word size
#endif
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
