{-|
  Copyright   :  (C) 2019, Foamspace corp
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  SPI master and slave core
-}
module Clash.Cores.SPI
  ( SPIMode (..)
    -- * SPI slave
  , SPISlaveConfig (..)
  , spiSlave
    -- * SPI master
  , spiMaster
  )
where

import Data.Maybe (fromMaybe, isJust)
import Clash.Prelude
import Clash.Sized.Internal.BitVector

-- | SPI mode
--
-- * CPOL: Clock POLarity
--
-- * CPHA: Clock PHAse
data SPIMode
  = SPIMode0
  -- ^ CPOL = 0, CPHA = 0
  --
  -- Clock is idle low, data is sampled on rising edge of SCK,
  -- data is shifted on falling edge of SCK.
  | SPIMode1
  -- ^ CPOL = 0, CPHA = 1
  --
  -- Clock is idle low, data is sampled on falling edge of SCK,
  -- data is shifted on rising edge of SCK.
  | SPIMode2
  -- ^ CPOL = 1, CPHA = 0
  --
  -- Clock is idle high, data is sampled on rising edge of SCK,
  -- data is shifted on falling edge of SCK.
  | SPIMode3
  -- ^ CPOL = 1, CPHA = 1
  --
  -- Clock is idle high, data is sampled on falling edge of SCK,
  -- data is shifted on falling edge of SCK.
  deriving (Eq)

-- | SPI slave configuration
data SPISlaveConfig ds dom
  = SPISlaveConfig
  { spiSlaveConfigMode :: SPIMode
  -- ^ SPI mode
  , spiSlaveConfigLatch :: Bool
  -- ^ Whether to latch the SPI pins, only set to False when the
  -- SCK frequency is lower than the Core clock.
  , spiSlaveConfigBuffer
      :: BiSignalIn ds dom 1
      -> Signal dom Bool
      -> Signal dom Bit
      -> BiSignalOut ds dom 1
  -- ^ Tri-state buffer: first argument is the inout pin, second
  -- argument is the output enable, third argument is the value to
  -- output when the enable is high
  }

-- | SPI capture a shift logic that is shared between slave and master
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

-- | SPI slave configurable SPI mode and tri-state buffer
spiSlave
  :: forall n ds dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => SPISlaveConfig ds dom
  -- ^ Configure SPI mode and tri-state buffer
  -> BiSignalIn ds dom 1
  -- ^ Inout port connected to the tri-state buffer
  -> Signal dom Bool
  -- ^ Slave select
  -> Signal dom Bit
  -- ^ MOSI
  -> Signal dom Bool
  -- ^ SCK
  -> Signal dom (BitVector n)
  -- ^ Input captured the moment slave select goes low
  -> ( BiSignalOut ds dom 1
     , Signal dom (Maybe (BitVector n)))
  -- ^ First part of the tuple can be ignored. Second part of the tuple
  -- is (Maybe) the word send by the master
spiSlave (SPISlaveConfig mode latch buf) bin ss mosi sck din =
  let ssL   = if latch then delay undefined ss   else ss
      mosiL = if latch then delay undefined mosi else mosi
      sckL  = if latch then delay undefined sck  else sck
      (miso,dout) = spiCommon mode ssL mosiL sckL din
      bout = buf bin (not <$> ssL) miso
  in  (bout,dout)

-- | SPI master configurable in the SPI mode and clock divider
--
-- Adds latch to MISO line if the (half period) clock divider is
-- set to 2 or higher.
spiMaster
  :: forall n m dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n, 1 <= m)
  => SPIMode
  -- ^ SPI Mode
  -> SNat m
  -- ^ Clock divider (half period)
  --
  -- If set to two or higher, the MISO line will be latched
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
  let (mosi,dout)   = spiCommon mode ssL misoL sckL
                        (fromMaybe undefined# <$> din)
      latch = snatToInteger fN /= 1
      ssL   = if latch then delay undefined ss   else ss
      misoL = if latch then delay undefined miso else miso
      sckL  = if latch then delay undefined sck  else sck
      (ss,sck,busy) = spiGen mode fN din
  in  (dout,busy,ss,mosi,sck)

-- | Generate slave select and SCK
spiGen
  :: forall n m dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n, 1 <= m)
  => SPIMode
  -> SNat m
  -> Signal dom (Maybe (BitVector n))
  -> ( Signal dom Bool
     , Signal dom Bool
     , Signal dom Bool
     )
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
