module Box where

import CLaSH.Prelude

topEntity :: BitVector 16 -> (BitVector 16,BitVector 8,Vec 8 Bit)
topEntity vec = (pack  tup
                ,pack  ((0 :: Bit) :> 0 :> 0 :> 0 :> 1 :> 1 :> 1 :> 1 :> Nil)
                ,unpack 0xF0
                )
  where
    tup :: (Vec 8 Bit, Vec 8 Bit)
    tup = unpack vec

testInput :: Signal (BitVector 16)
testInput = signal (0x00FF)

expectedOutput :: Signal (BitVector 16,BitVector 8,Vec 8 Bit) -> Signal Bool
expectedOutput = outputVerifier ((0x00FF,0x0F,1:>1:>1:>1:>0:>0:>0:>0:>Nil):>Nil)
