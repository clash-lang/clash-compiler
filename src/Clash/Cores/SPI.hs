module Clash.Cores.SPI where

import Clash.Prelude
import Clash.Sized.Internal.BitVector

data SPIMode = SPIMode0 | SPIMode1 | SPIMode2 | SPIMode3
  deriving Eq

data SPISlaveConfig
  = SPISlaveConfig
  { spiSlaveConfigMode :: SPIMode
  }

spiSlave
  :: forall n dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => SPISlaveConfig
  -> Signal dom Bool
  -- ^ Slave select
  -> Signal dom Bit
  -- ^ MOSI
  -> Signal dom Bool
  -- ^ SCK
  -> Signal dom (BitVector n)
  -- ^ DIN
  -> Signal dom ( Bit -- MISO
                , Maybe (BitVector n) -- DOUT
                )
spiSlave (SPISlaveConfig mode) ss mosi sck din =
  moore go cvt ((0 :: Index n,undefined,unpack undefined#),(repeat 1,False,0))
               (bundle ( delay False ss
                       , delay undefined mosi
                       , delay undefined sck
                       , din ))
 where
  cvt (_,(miso,done,dout)) = (head miso,if done then Just dout else Nothing)

  go ((bitCntQ,sckOldQ,dataQ),(misoQ,_doneQ,doutQ)) (ssQ,mosiQ,sckQ,dinI)
    = ((bitCntD,sckQ,dataD),(misoD,doneD,doutD))
    where
      bitCntD
        | ssQ       = 0
        | sampleSck = if bitCntQ == maxBound then 0 else bitCntQ + 1
        | otherwise = bitCntQ

      dataD
        | ssQ       = unpack dinI
        | sampleSck = if bitCntQ == maxBound
                      then unpack dinI
                      else tail dataQ :< mosiQ
        | otherwise = dataQ

      doutD
        | doneD
        = pack (tail dataQ :< mosiQ)
        | otherwise
        = doutQ

      misoD
        | ssQ
        = unpack dinI
        | shiftSck
        = tail @(n-1) misoQ :< 1
        | otherwise
        = misoQ

      doneD = not ssQ && sampleSck && bitCntQ == maxBound

      risingSck  = not sckOldQ && sckQ
      fallingSck = sckOldQ && not sckQ

      sampleSck = if mode == SPIMode0 || mode == SPIMode3
                  then risingSck else fallingSck
      shiftSck = if mode == SPIMode1 || mode == SPIMode2
                 then risingSck else fallingSck
