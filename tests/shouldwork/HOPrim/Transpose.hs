module Transpose where

import CLaSH.Prelude

topEntity = transposeV

transposeV :: Vec 2 (Vec 3 Int) -> Vec 3 (Vec 2 Int)
transposeV = sequenceA

testInput :: Signal (Vec 2 (Vec 3 Int))
testInput = pure ((1:>2:>3:>Nil):>(4:>5:>6:>Nil):>Nil)

expectedOutput :: Signal (Vec 3 (Vec 2 Int)) -> Signal Bool
expectedOutput = outputVerifier (((1:>4:>Nil):>(2:>5:>Nil):>(3:>6:>Nil):>Nil):>Nil)
