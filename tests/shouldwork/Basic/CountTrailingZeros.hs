{-# LANGUAGE CPP, MagicHash #-}
module CountTrailingZeros where

#include "MachDeps.h"

import CLaSH.Prelude
import GHC.Word
import Data.Bits

topEntity :: Word -> Int
topEntity = countTrailingZeros

testInput :: Signal Word
testInput = stimuliGenerator $(v [1::Word,3,8,50,0])

expectedOutput :: Signal Int -> Signal Bool
#if WORD_SIZE_IN_BITS == 64
expectedOutput = outputVerifier $(v ([0,0,3,1,64]::[Int]))
#elif WORD_SIZE_IN_BITS == 32
expectedOutput = outputVerifier $(v ([0,0,3,1,32]::[Int]))
#else
#error Unsupported word size
#endif
