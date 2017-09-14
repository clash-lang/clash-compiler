module MovingAvg where

import Clash.Prelude

windowN
  :: (Default a,KnownNat n, HasClockReset domain gated synchronous)
  => SNat (n+1)
  -> Signal domain a
  -> Vec (n + 1) (Signal domain a)
windowN size = window

movingAvarageNaive size signal =  fold (+) <$> bundle (windowN size signal)

topEntity
  :: SystemClockReset
  => Signal System (Signed 9)
  -> Signal System (Signed 9)
topEntity = movingAvarageNaive d5
