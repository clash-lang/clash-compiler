module FindIndex where

import CLaSH.Prelude

topEntity :: Vec 7 (Unsigned 8) -> Maybe (Index 7)
topEntity = findIndex (> 3)

testInput :: Signal (Vec 7 (Unsigned 8))
testInput = pure (1:>3:>2:>4:>3:>5:>6:>Nil)

expectedOutput :: Signal (Maybe (Index 7)) -> Signal Bool
expectedOutput = outputVerifier ((Just 3) :> Nil)
