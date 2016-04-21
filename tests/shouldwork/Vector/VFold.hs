module VFold where

import CLaSH.Prelude

csSort = vfold (const csRow)
  where
    cs a b     = if a > b then (a,b) else (b,a)
    csRow y xs = let (y',xs') = mapAccumL cs y xs in xs' :< y'

topEntity :: Vec 4 Int -> Vec 4 Int
topEntity = csSort

testInput :: Signal (Vec 4 Int)
testInput = pure (7 :> 3 :> 9 :> 1 :> Nil)

expectedOutput :: Signal (Vec 4 Int) -> Signal Bool
expectedOutput = outputVerifier ((1:>3:>7:>9:>Nil):>Nil)
