module Minimum where

import CLaSH.Prelude

topEntity :: Vec 3 Int -> Int
topEntity = minimum

testInput :: Signal (Vec 3 Int)
testInput = signal (4 :> 8 :> (-2) :> Nil)

expectedOutput :: Signal Int -> Signal Bool
expectedOutput = outputVerifier (singleton (-2))
