module ToList where

import CLaSH.Prelude
import qualified Data.List as L

topEntity :: Vec 3 Int -> Int
topEntity xs = L.foldr (+) 0 (toList xs)

testInput :: Signal (Vec 3 Int)
testInput = pure (1 :> 2 :> 3 :> Nil)

expectedOutput :: Signal Int -> Signal Bool
expectedOutput = outputVerifier (6 :> Nil)
