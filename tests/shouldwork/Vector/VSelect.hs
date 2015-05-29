module VSelect where

import CLaSH.Prelude

topEntity :: Vec 8 Int -> Vec 4 Int
topEntity x = select d1 d2 d4 x

testInput :: Signal (Vec 8 Int )
testInput = signal (iterateI (+1) 1)

expectedOutput :: Signal (Vec 4 Int) -> Signal Bool
expectedOutput = outputVerifier ((2:>4:>6:>8:>Nil):>Nil)
