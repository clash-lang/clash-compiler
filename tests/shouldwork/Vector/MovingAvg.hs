module MovingAvg where

import CLaSH.Prelude

windowN :: (Default a,KnownNat n) => SNat (n+1) -> Signal a -> Vec (n + 1) (Signal a)
windowN size = window

movingAvarageNaive size signal =  fold (+) <$> bundle (windowN size signal)

topEntity :: Signal (Signed 9) -> Signal (Signed 9)
topEntity = movingAvarageNaive d5
