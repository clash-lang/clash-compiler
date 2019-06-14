module MovingAvg where

import Clash.Prelude

windowN
  :: HiddenClockResetEnable dom conf
  => Default a
  => KnownNat n
  => Undefined a
  => SNat (n+1)
  -> Signal dom a
  -> Vec (n + 1) (Signal dom a)
windowN size = window

movingAvarageNaive size signal =  fold (+) <$> bundle (windowN size signal)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Signed 9)
  -> Signal System (Signed 9)
topEntity = exposeClockResetEnable (movingAvarageNaive d5)
