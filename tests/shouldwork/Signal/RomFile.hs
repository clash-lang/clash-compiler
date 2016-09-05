module RomFile where

import CLaSH.Prelude

zeroAt0 :: Signal (Unsigned 8) -> Signal (Unsigned 8)
zeroAt0 a = mux en a 0
  where
    en = register False (signal True)

topEntity :: Signal (Unsigned 8) -> Signal (Unsigned 8)
topEntity rd = zeroAt0 (unpack <$> dout)
  where
    dout = romFilePow2 "memory.list" rd

testInput :: Signal (Unsigned 8)
testInput = cnt
  where
    cnt = register 0 (cnt + 1)

expectedOutput :: Signal (Unsigned 8) -> Signal Bool
expectedOutput = outputVerifier $(listToVecTH [0::Unsigned 8,0,1,2,3,4,5,6,7,8])
