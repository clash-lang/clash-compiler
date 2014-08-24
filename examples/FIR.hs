module FIR where

import CLaSH.Prelude

dotp :: SaturatingNum a
     => Vec n a
     -> Vec n a
     -> a
dotp as bs = foldl boundedPlus 0 (zipWith boundedMult as bs)

fir :: (Default a, KnownNat n, SaturatingNum a)
    => Vec (n + 1) (Signal a) -> Signal a -> Signal a
fir coeffs x_t = y_t
  where
    y_t = dotp coeffs xs
    xs  = window x_t

topEntity :: Signal (Signed 16) -> Signal (Signed 16)
topEntity = fir $(v [2::Signal (Signed 16),3,-2,8])

testInput :: Signal (Signed 16)
testInput = stimuliGenerator $(v [2::Signed 16,3,-2,8])

expectedOutput :: Signal (Signed 16) -> Signal Bool
expectedOutput = outputVerifier $(v [4::Signed 16,12,1,20])
