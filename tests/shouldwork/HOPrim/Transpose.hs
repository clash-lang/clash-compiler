module Transpose where

import CLaSH.Prelude

topEntity = transposeV

transposeV :: Vec 3 (Vec 4 Int) -> Vec 4 (Vec 3 Int)
transposeV = sequenceA

testInput :: Signal (Vec 3 (Vec 4 Int))
testInput = pure ((1:>2:>3:>4:>Nil):>(5:>6:>7:>8:>Nil):>(9:>10:>11:>12:>Nil):>Nil)

expectedOutput :: Signal (Vec 4 (Vec 3 Int)) -> Signal Bool
expectedOutput = outputVerifier ((transpose ((1:>2:>3:>4:>Nil):>(5:>6:>7:>8:>Nil):>(9:>10:>11:>12:>Nil):>Nil)):>Nil)
