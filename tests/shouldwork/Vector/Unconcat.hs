module Unconcat where

import CLaSH.Prelude

topEntity :: Vec 6 (Unsigned 8) -> Vec 2 (Vec 3 (Unsigned 8))
topEntity = unconcatI

testInput :: Signal (Vec 6 (Unsigned 8))
testInput = pure (1 :> 2 :> 3 :> 4 :> 5 :> 6 :> Nil)

expectedOutput :: Signal (Vec 2 (Vec 3 (Unsigned 8)))
               -> Signal Bool
expectedOutput = outputVerifier (((1 :> 2 :> 3 :> Nil) :> (4 :> 5 :> 6 :> Nil) :> Nil):>Nil)
