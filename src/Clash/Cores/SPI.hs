module Clash.Cores.SPI where

import Data.Maybe (isJust)
import Clash.Prelude
import Clash.Sized.Internal.BitVector

data SPIMode = SPIMode0 | SPIMode1 | SPIMode2 | SPIMode3
  deriving (Eq)

data SPISlaveConfig ds dom
  = SPISlaveConfig
  { spiSlaveConfigMode :: SPIMode
  , spiSlaveConfigBuffer
      :: BiSignalIn ds dom 1
      -> Signal dom Bool
      -> Signal dom Bit
      -> BiSignalOut ds dom 1
  }

spiSlave
  :: forall n ds dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => SPISlaveConfig ds dom
  -> BiSignalIn ds dom 1
  -> Signal dom Bool
  -- ^ Slave select
  -> Signal dom Bit
  -- ^ MOSI
  -> Signal dom Bool
  -- ^ SCK
  -> Signal dom (BitVector n)
  -- ^ DIN
  -> (BiSignalOut ds dom 1, Signal dom (Maybe (BitVector n)))
spiSlave (SPISlaveConfig mode buf) bin ss mosi sck din =
  let ssN = delay undefined ss
      (miso,dout) = mooreB go cvt
                      ( (0 :: Index n,undefined,unpack undefined#)
                      , (repeat 1,False,0))
                      ( ssN
                      , delay undefined mosi
                      , delay undefined sck
                      , din
                      )
      bout = buf bin (not <$> ssN) miso
  in  (bout,dout)
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

spiMaster
  :: forall n dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => SPIMode
  -> Signal dom Bit
  -- ^ MISO
  -> Signal dom (Maybe (BitVector n))
  -- ^ Data: Master -> Slave
  -> ( Signal dom (Maybe (BitVector n)) -- Data: Slave -> Master
     , Signal dom Bool -- Busy
     , Signal dom Bit -- MOSI
     , Signal dom Bool -- SCK
     , Signal dom Bool -- SS
     )
spiMaster _mode misoI dinI =
  mooreB go cvt ( 0 :: Index n    -- cntR
                , unpack 0        -- dataR
                , 0 :: Unsigned 1 -- sckR
                , 0               -- mosiR
                , Idle            -- stateR
                , 0               -- dataOutR
                , False           -- newDataR
                )
                (misoI, dinI)
 where
  cvt (_cntQ,_dataQ,sckQ,mosiQ,stateQ,dataOutQ,newDataQ)
    = ( if newDataQ then Just dataOutQ else Nothing
      , stateQ /= Idle
      , mosiQ
      , msb sckQ == low && stateQ == Transfer
      , stateQ == Idle
      )

  go (cntQ,dataQ,sckQ,mosiQ,stateQ,dataOutQ,_newDataQ) (miso,din) =
    (cntD,dataD,sckD,mosiD,stateD,dataOutD,newDataD)
   where
    sckD = case stateQ of
      Idle -> 0
      WaitHalf -> if sckQ == 0 then 0 else sckQ + 1
      _ -> sckQ + 1

    dataD = case stateQ of
      Idle     -> maybe dataQ unpack din
      Transfer -> if sckQ == 0 then tail @(n-1) dataQ :< miso else dataQ
      _        -> dataQ

    mosiD = case stateQ of
      Transfer | sckQ == 0 -> head dataQ
      _ -> mosiQ

    cntD = case stateQ of
      Idle -> 0
      Transfer | sckQ == 1 -> cntQ + 1
      _ -> cntQ

    newDataD = case stateQ of
      Transfer -> cntQ == maxBound
      _ -> False

    dataOutD = case stateQ of
      Transfer | cntQ == maxBound -> pack dataQ
      _ -> dataOutQ

    stateD = case stateQ of
      Idle | isJust din -> WaitHalf
      WaitHalf | sckQ == 0 -> Transfer
      Transfer | cntQ == maxBound -> Idle
      _ -> stateQ

data SPIMasterState
  = Idle
  | WaitHalf
  | Transfer
  deriving (Eq, Generic, NFDataX)
