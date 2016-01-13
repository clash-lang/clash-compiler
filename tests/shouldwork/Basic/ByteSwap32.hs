{-# LANGUAGE MagicHash #-}
module ByteSwap32 where

import CLaSH.Prelude
import GHC.Word
import Data.Bits

topEntity :: Word32 -> Word32
topEntity = byteSwap32

testInput :: Signal Word32
testInput = stimuliGenerator $(v [1::Word32,3,8,50,0])

expectedOutput :: Signal Word32 -> Signal Bool
expectedOutput = outputVerifier $(v ([16777216,50331648,134217728,838860800,0]::[Word32]))
