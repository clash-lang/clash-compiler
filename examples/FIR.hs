module FIR where

import CLaSH.Prelude

dotp :: Num a
     => Vec n a
     -> Vec n a
     -> a
dotp as bs = foldl (+) 0 (zipWith (*) as bs)

fir :: (Default a, KnownNat n, Num a)
    => Vec (n + 1) (Signal a) -> Signal a -> Signal a
fir coeffs x_t = y_t
  where
    y_t = dotp coeffs xs
    xs  = window x_t

topEntity :: Signal (Signed 16) -> Signal (Signed 16)
topEntity = fir $(v [0::Signal (Signed 16),1,2,3])
