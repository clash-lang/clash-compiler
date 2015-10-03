module FIR where

import CLaSH.Prelude

dotp :: SaturatingNum a
     => Vec (n + 1) a
     -> Vec (n + 1) a
     -> a
dotp as bs = fold boundedPlus (zipWith boundedMult as bs)

fir :: (Default a, KnownNat n, SaturatingNum a)
    => Vec (n + 1) (Signal a) -> Signal a -> Signal a
fir coeffs x_t = y_t
  where
    y_t = dotp coeffs xs
    xs  = window x_t

topEntity :: Signal (Signed 16) -> Signal (Signed 16)
topEntity = fir (2:>3:>(-2):>8:>Nil)

testInput :: Signal (Signed 16)
testInput = stimuliGenerator (2:>3:>(-2):>8:>Nil)

expectedOutput :: Signal (Signed 16) -> Signal Bool
expectedOutput = outputVerifier (4:>12:>1:>20:>Nil)
