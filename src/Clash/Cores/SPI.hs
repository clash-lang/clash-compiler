module Clash.Cores.SPI where

import Clash.Prelude

spiSlave
  :: forall dom n
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => Signal dom Bool
  -- ^ Slave select
  -> Signal dom Bit
  -- ^ MOSI
  -> Signal dom Bool
  -- ^ SCK
  -> Signal dom (BitVector n)
  -- ^ DIN
  -> Signal dom ( Bit -- MISO
                , Bool -- done
                , BitVector n -- DOUT
                )
spiSlave ss mosi sck din =
  moore go snd ((0 :: Index n,undefined,undefined),(0,False,0))
               (bundle ( delay False ss
                       , delay undefined mosi
                       , delay undefined sck
                       , din ))
 where
  go ((bitCntQ,sckOldQ,dataQ),(misoQ,doneQ,doutQ)) (ssQ,mosiQ,sckQ,dinI)
    = ((bitCntD,sckQ,dataD),(misoD,doneD,doutD))
    where
      bitCntD
        | ssQ       = 0
        | risingSck = if bitCntQ == maxBound then 0 else bitCntD + 1
        | otherwise = bitCntD

      dataD
        | ssQ       = unpack dinI
        | risingSck = if bitCntQ == maxBound
                      then unpack dinI
                      else init dataQ :< mosiQ
        | otherwise = dataD

      doutD
        | doneD
        = pack (init dataQ :< mosiQ)
        | otherwise
        = doutQ

      misoD
        | not ssQ && fallingSck = head @(n-1) dataQ
        | otherwise  = misoQ

      doneD = not ssQ && risingSck && bitCntQ == maxBound

      risingSck  = not sckOldQ && sckQ
      fallingSck = sckOldQ && not sckQ
