module VMerge where

import CLaSH.Prelude

topEntity :: (Vec 2 Int,Vec 2 Int) -> Vec 4 Int
topEntity (x,y) = merge x y

testInput :: Signal (Vec 2 Int,Vec 2 Int)
testInput = signal (iterateI (+1) 1,iterateI (+1) 3)

expectedOutput :: Signal (Vec 4 Int) -> Signal Bool
expectedOutput = outputVerifier ((1:>3:>2:>4:>Nil):>Nil)
