module Fold where

import CLaSH.Prelude

topEntity :: Vec 8 Int -> Int
topEntity = fold (+)

testInput :: Signal (Vec 8 Int)
testInput = pure (1:>2:>3:>4:>5:>6:>7:>8:>Nil)

expectedOutput :: Signal Int -> Signal Bool
expectedOutput = outputVerifier (36 :> Nil)
