module Clash.Cores.SPI where

import Data.Maybe (fromMaybe, isJust)
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

spiCommon
  :: forall n dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => SPIMode
  -> Signal dom Bool
  -- ^ Slave select
  -> Signal dom Bit
  -- ^ Slave: MOSI; Master: MISO
  -> Signal dom Bool
  -- ^ SCK
  -> Signal dom (BitVector n)
  -> ( Signal dom Bit -- Slave: MISO; Master: MOSI
     , Signal dom (Maybe (BitVector n))
     )
spiCommon mode ssI msI sckI dinI =
  mooreB go cvt ( 0 :: Index n      -- cntR
                , undefined         -- sckOldR
                , unpack undefined# -- dataInR
                , unpack undefined# -- dataOutR
                , False             -- doneR
                )
                (ssI,msI,sckI,dinI)
 where
  cvt (_,_,dataInQ,dataOutQ,doneQ) =
    ( head dataOutQ
    , if doneQ
         then Just (pack dataInQ)
         else Nothing
    )

  go (cntQ,sckOldQ,dataInQ,dataOutQ,_) (ss,ms,sck,din) =
    (cntD,sck,dataInD,dataOutD,doneD)
   where
    cntD
      | ss        = 0
      | sampleSck = if cntQ == maxBound then 0 else cntQ + 1
      | otherwise = cntQ

    dataInD
      | ss        = unpack undefined#
      | sampleSck = tail @(n-1) dataInQ :< ms
      | otherwise = dataInQ

    dataOutD
      | ss        = unpack din
      | shiftSck  = if cntQ == maxBound && cntD == 0
                       then unpack din
                       else if (mode == SPIMode1 || mode == SPIMode3) && cntQ == 0
                               then dataOutQ
                               else tail @(n-1) dataOutQ :< unpack undefined#
      | otherwise = dataOutQ

    doneD = not ss && sampleSck && cntQ == maxBound

    risingSck  = not sckOldQ && sck
    fallingSck = sckOldQ && not sck

    sampleSck = if mode == SPIMode0 || mode == SPIMode3
                then risingSck else fallingSck

    shiftSck  = if mode == SPIMode1 || mode == SPIMode2
                then risingSck else fallingSck


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
      (miso,dout) = spiCommon mode ssN (delay undefined mosi)
                                       (delay undefined sck)
                                       din
      bout = buf bin (not <$> ssN) miso
  in  (bout,dout)

spiMaster
  :: forall n m dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n, 2 <= m)
  => SPIMode
  -- ^ SPI Mode
  -> SNat m
  -- ^ Clock divider
  -> Signal dom Bit
  -- ^ MISO
  -> Signal dom (Maybe (BitVector n))
  -- ^ Data: Master -> Slave
  -> ( Signal dom (Maybe (BitVector n)) -- Data: Slave -> Master
     , Signal dom Bool -- Busy
     , Signal dom Bool -- SS
     , Signal dom Bit  -- MOSI
     , Signal dom Bool -- SCK
     )
spiMaster mode fN miso din =
  let (mosi,dout)   = spiCommon mode
                        (delay undefined ss)
                        (delay undefined miso)
                        (delay undefined sck)
                        (fromMaybe undefined# <$> din)
      (ss,sck,busy) = spiGen mode fN din
  in  (dout,busy,ss,mosi,sck)

spiGen
  :: forall n m dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n, 2 <= m)
  => SPIMode
  -> SNat m
  -> Signal dom (Maybe (BitVector n))
  -> (Signal dom Bool, Signal dom Bool, Signal dom Bool)
spiGen mode SNat = unbundle . moore go cvt (0 :: Index (2*n),False,Idle @ m)
 where
  cvt (_,sck,st) =
    ( st == Idle
    , if mode == SPIMode0 || mode == SPIMode1
      then sck
      else not sck
    , st /= Idle)

  go (cntQ,sckQ,stQ) din = (cntD,sckD,stD)
   where
    stD = case stQ of
      Idle | isJust din -> Wait
      Wait -> Transfer 0

      Transfer n
        | n /= maxBound -> Transfer (n+1)
        | cntQ == maxBound -> Finish
        | otherwise -> Transfer 0
      Finish -> Idle
      _ -> stQ

    cntD = case stQ of
      Transfer n
        | n == maxBound -> if cntQ == maxBound then 0 else cntQ+1
        | otherwise -> cntQ
      _ -> 0

    sckD = case stQ of
      Wait | mode == SPIMode1 || mode == SPIMode3 -> not sckQ
      Transfer n | n == maxBound -> not sckQ
      _ -> sckQ

data SPIMasterState n
  = Idle
  | Wait
  | Transfer (Index n)
  | Finish
  deriving (Eq, Generic, NFDataX)
