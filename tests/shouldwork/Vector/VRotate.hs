module VRotate where

import CLaSH.Prelude

topEntity :: Vec 5 Int -> (Vec 5 Int,Vec 5 Int)
topEntity v = (rotateLeftS v d2,rotateRightS v d2)

testInput :: Signal (Vec 5 Int)
testInput = signal (1:>2:>3:>4:>5:>Nil)

expectedOutput :: Signal (Vec 5 Int,Vec 5 Int) -> Signal Bool
expectedOutput = outputVerifier ((3:>4:>5:>1:>2:>Nil,4:>5:>1:>2:>3:>Nil):>Nil)
