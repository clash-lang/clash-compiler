module Scatter where

import CLaSH.Prelude

topEntity :: Vec 5 (Unsigned 10) -> Vec 5 (Unsigned 10)
topEntity = scatter defvec to
  where
    defvec = replicate d5 99
    to = 0 :> 4 :> 2 :> 3 :> 1 :> Nil

testInput :: Signal (Vec 5 (Unsigned 10))
testInput = stimuliGenerator
  ((1 :> 2 :> 3 :> 4 :> 5 :> Nil) :> Nil)

expectedOutput :: Signal (Vec 5 (Unsigned 10)) -> Signal Bool
expectedOutput = outputVerifier
  ((1 :> 5 :> 3 :> 4 :> 2 :> Nil) :> Nil)
