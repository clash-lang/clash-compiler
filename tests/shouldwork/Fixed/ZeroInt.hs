module ZeroInt where

import CLaSH.Prelude

topEntity :: (UFixed 0 8,UFixed 0 8) -> UFixed 0 8
topEntity = uncurry (*)

testInput :: Signal (UFixed 0 8, UFixed 0 8)
testInput = pure (0.2,0.35)

expectedOutput :: Signal (UFixed 0 8) -> Signal Bool
expectedOutput = outputVerifier $(listToVecTH [0.06640625 :: UFixed 0 8])
