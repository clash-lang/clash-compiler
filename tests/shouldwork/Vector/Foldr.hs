module Foldr where

import CLaSH.Prelude

topEntity :: Vec 4 (Unsigned 8) -> (Unsigned 8)
topEntity = foldr div 1

testInput :: Signal (Vec 4 (Unsigned 8))
testInput = pure (24 :> 7 :> 4 :> 2 :> Nil)

expectedOutput :: Signal (Unsigned 8) -> Signal Bool
expectedOutput = outputVerifier (8 :> Nil)
