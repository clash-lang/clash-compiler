module VReverse where

import CLaSH.Prelude

topEntity :: Vec 4 Int -> Vec 4 Int
topEntity = reverse

testInput :: Signal (Vec 4 Int )
testInput = signal (iterateI (+1) 1)

expectedOutput :: Signal (Vec 4 Int) -> Signal Bool
expectedOutput = outputVerifier ((4:>3:>2:>1:>Nil):>Nil)
