{-|
  Copyright   :  (C) 2019, Foamspace corp
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  SPI master and slave core
-}
module Clash.Cores.SPI
  ( SPIMode(..)
    -- * SPI master
  , spiMaster
    -- * SPI slave
  , SPISlaveConfig(..)
  , spiSlave
    -- ** Vendor configured SPI slaves
  , spiSlaveLatticeSBIO
  ) where

import Data.Maybe (fromMaybe, isJust)
import Test.QuickCheck as QC

import Clash.Prelude
import Clash.Sized.Internal.BitVector

import Clash.Cores.LatticeSemi.IO

-- | SPI mode
--
-- * CPOL: Clock POLarity
-- * CPHA: Clock PHAse
data SPIMode
  = SPIMode0
  -- ^ CPOL = 0, CPHA = 0
  --
  -- Clock is idle low, so the leading edge is rising.
  -- Phase is low, so data is sampled on the leading edge.
  --
  -- TL;DR Data is shifted on the rising edge of SCK.
  | SPIMode1
  -- ^ CPOL = 0, CPHA = 1
  --
  -- Clock is idle low, so the leading edge is rising.
  -- Phase is high, so data is sampled on the trailing edge.
  --
  -- TL;DR Data is shifted on the falling edge of SCK.
  | SPIMode2
  -- ^ CPOL = 1, CPHA = 0
  --
  -- Clock is idle high, so the leading edge is falling.
  -- Phase is low, so data is sampled on the leading edge.
  --
  -- TL;DR Data is shifted on the falling edge of SCK.
  | SPIMode3
  -- ^ CPOL = 1, CPHA = 1
  --
  -- Clock is idle high, so the leading edge is falling.
  -- Phase is high, so data is sampled on the trailing edge.
  --
  -- TL;DR Data is shifted on the rising edge of SCK.
  deriving (Eq, Show)

instance Arbitrary SPIMode where
  arbitrary = QC.elements [SPIMode0, SPIMode1, SPIMode2, SPIMode3]

idleOnLow :: SPIMode -> Bool
idleOnLow SPIMode0 = True
idleOnLow SPIMode1 = True
idleOnLow _        = False

sampleOnRising :: SPIMode -> Bool
sampleOnRising SPIMode0 = True
sampleOnRising SPIMode3 = True
sampleOnRising _        = False

sampleOnLeading :: SPIMode -> Bool
sampleOnLeading SPIMode0 = True
sampleOnLeading SPIMode2 = True
sampleOnLeading _        = False

sampleOnTrailing :: SPIMode -> Bool
sampleOnTrailing = not . sampleOnLeading

data SPISlaveConfig ds dom
  = SPISlaveConfig
  { spiSlaveConfigMode :: SPIMode
  -- ^ SPI mode
  , spiSlaveConfigLatch :: Bool
  -- ^ Whether to latch the SPI pins
  --
  -- Recommended:
  --
  -- * Set to /True/ when core clock is /more/ than twice as fast as the SCK
  --   Clock: 2*SCK < Core
  --
  -- * Set to /False/ when core clock is twice as fast, or as fast, as the SCK
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
  -> ( Signal dom Bit   -- Slave: MISO; Master: MOSI
     , Signal dom Bool  -- Acknowledge start of transfer
     , Signal dom (Maybe (BitVector n))
     )
spiCommon mode ssI msI sckI dinI =
  mooreB go cvt ( 0 :: SatIndex 'SatWrap n -- cntR
                , False                    -- cntOldR
                , undefined                -- sckOldR
                , deepErrorX "no initial dataInR"
                , deepErrorX "no initial dataOutR"
                , False                    -- ackR
                , False                    -- doneR
                )
                (ssI,msI,sckI,dinI)
 where
  cvt (_,_,_,dataInQ,dataOutQ,ackQ,doneQ) =
    ( head dataOutQ
    , ackQ
    , if doneQ
         then Just (pack dataInQ)
         else Nothing
    )

  go (cntQ,cntOldQ,sckOldQ,dataInQ,dataOutQ,_,_) (ss,ms,sck,din) =
    (cntD,cntOldD,sck,dataInD,dataOutD,ackD,doneD)
   where
    cntD
      | ss        = 0
      | sampleSck = succ cntQ
      | otherwise = cntQ

    dataInD
      | ss        = unpack undefined#
      | sampleSck = tail @(n-1) dataInQ :< ms
      | otherwise = dataInQ

    dataOutD
      | ss        = unpack din
      | shiftSck  = if cntOldQ
                       then unpack din
                       else if sampleOnTrailing mode && cntQ == 0
                               then dataOutQ
                               else tail @(n-1) dataOutQ :< unpack undefined#
      | otherwise = dataOutQ

    -- The counter is updated during the capture moment
    -- But we need to know during the shift moment whether the counter
    -- overflowed to determine whether we need to load new data or shift
    -- the existing data. That's why we capture it here.
    cntOldD | not ss && shiftSck = cntQ == maxBound
            | otherwise          = cntOldQ

    ackD  = not ss && shiftSck && cntQ == 1
    doneD = not ss && sampleSck && cntQ == maxBound

    (sampleSck, shiftSck)
      | sampleOnRising mode = (risingSck, fallingSck)
      | otherwise           = (fallingSck, risingSck)
     where
      risingSck  = not sckOldQ && sck
      fallingSck = sckOldQ && not sck

-- | SPI slave configurable SPI mode and tri-state buffer
spiSlave
  :: forall n ds dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => SPISlaveConfig ds dom
  -- ^ Configure SPI mode and tri-state buffer
  -> Signal dom Bool
  -- ^ Serial Clock (SCLK)
  -> Signal dom Bit
  -- ^ Master Output Slave Input (MOSI)
  -> BiSignalIn ds dom 1
  -- ^ Master Input Slave Output (MISO)
  --
  -- Inout port connected to the tri-state buffer for the MISO
  -> Signal dom Bool
  -- ^ Slave select (SS)
  -> Signal dom (BitVector n)
  -- ^ Data to send from master to slave
  --
  -- Input is latched the moment slave select goes low
  -> ( BiSignalOut ds dom 1
     , Signal dom Bool
     , Signal dom (Maybe (BitVector n)))
  -- ^ Parts of the tuple:
  --
  -- 1. The "out" part of the inout port of the MISO; used only for simulation.
  --
  -- 2. (Maybe) the word send by the master
spiSlave (SPISlaveConfig mode latch buf) sclk mosi bin ss din =
  let ssL   = if latch then delay undefined ss   else ss
      mosiL = if latch then delay undefined mosi else mosi
      sclkL = if latch then delay undefined sclk else sclk
      (miso, ack, dout) = spiCommon mode ssL mosiL sclkL din
      bout = buf bin (not <$> ssL) miso
  in  (bout, ack, dout)

-- | SPI master configurable in the SPI mode and clock divider
--
-- Adds latch to MISO line if the (half period) clock divider is
-- set to 2 or higher.
spiMaster
  :: forall n halfPeriod waitTime dom
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     , 1 <= halfPeriod
     , 1 <= waitTime )
  => SPIMode
  -- ^ SPI Mode
  -> SNat halfPeriod
  -- ^ Clock divider (half period)
  --
  -- If set to two or higher, the MISO line will be latched
  -> SNat waitTime
  -- ^ (core clock) cycles between de-asserting slave-select and start of
  -- the SPI clock
  -> Signal dom (Maybe (BitVector n))
  -- ^ Data to send from master to slave, transmission starts when receiving
  -- /Just/ a value
  -> Signal dom Bit
  -- ^ Master Input Slave Output (MISO)
  -> ( Signal dom Bool -- SCK
     , Signal dom Bit  -- MOSI
     , Signal dom Bool -- SS
     , Signal dom Bool -- Busy
     , Signal dom Bool -- Acknowledge
     , Signal dom (Maybe (BitVector n)) -- Data: Slave -> Master
     )
  -- ^ Parts of the tuple:
  --
  -- 1. Serial Clock (SCLK)
  -- 2. Master Output Slave Input (MOSI)
  -- 3. Slave select (SS)
  -- 4. Busy signal indicating that a transmission is in progress, new words on
  --    the data line will be ignored when /True/
  -- 5. (Maybe) the word send from the slave to the master
spiMaster mode fN fW din miso =
  let (mosi, ack, dout)   = spiCommon mode ssL misoL sclkL
                        (fromMaybe undefined# <$> din)
      latch = snatToInteger fN /= 1
      ssL   = if latch then delay undefined ss   else ss
      misoL = if latch then delay undefined miso else miso
      sclkL = if latch then delay undefined sclk else sclk
      (ss, sclk, busy) = spiGen mode fN fW din
  in  (sclk, mosi, ss, busy, ack, dout)

-- | Generate slave select and SCK
spiGen
  :: forall n halfPeriod waitTime dom
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     , 1 <= halfPeriod
     , 1 <= waitTime )
  => SPIMode
  -> SNat halfPeriod
  -> SNat waitTime
  -> Signal dom (Maybe (BitVector n))
  -> ( Signal dom Bool
     , Signal dom Bool
     , Signal dom Bool
     )
spiGen mode SNat SNat =
  unbundle . moore go cvt (0 :: SatIndex 'SatWrap (2*n),False,Idle @halfPeriod @waitTime)
 where
  cvt (_, sck, st) =
    ( st == Idle
    , if idleOnLow mode then sck else not sck
    , st /= Idle
    )

  go (cntQ,sckQ,stQ) din = (cntD,sckD,stD)
   where
    stD = case stQ of
      Idle
        | isJust din -> Wait maxBound

      Wait 0 -> Transfer 0
      Wait w -> Wait (w - 1)


      Transfer n
        | n /= maxBound -> Transfer (n+1)
        | cntQ == maxBound -> Finish
        | otherwise -> Transfer 0
      Finish -> Idle
      _ -> stQ

    cntD = case stQ of
      Transfer n
        | n == maxBound -> succ cntQ
        | otherwise -> cntQ
      _ -> 0

    sckD = case stQ of
      Wait 0 | sampleOnTrailing mode -> not sckQ
      Transfer n | n == maxBound -> not sckQ
      _ -> sckQ

data SPIMasterState halfPeriod waitTime
  = Idle
  | Wait (SatIndex 'SatError waitTime)
  | Transfer (SatIndex 'SatError halfPeriod)
  | Finish
  deriving (Eq, Generic, NFDataX)

-- | SPI slave configurable SPI mode, using the SB_IO tri-state buffer
-- found on Lattice Semiconductor FPGAs
spiSlaveLatticeSBIO
  :: forall dom n
   . (HiddenClockResetEnable dom, 1 <= n, KnownNat n)
  => SPIMode
  -- ^ SPI Mode
  -> Bool
  -- ^ Whether to latch the SPI pins
  --
  -- Recommended:
  --
  -- * Set to /True/ when core clock is /more/ than twice as fast as the SCK
  --   Clock: 2*SCK < Core
  --
  -- * Set to /False/ when core clock is twice as fast, or as fast, as the SCK
  -> Signal dom Bool
  -- ^ Serial Clock (SCLK)
  -> Signal dom Bit
  -- ^ Master Output Slave Input (MOSI)
  -> BiSignalIn 'Floating dom 1
  -- ^ Master Input Slave Output (MISO)
  --
  -- Inout port connected to the tri-state buffer for the MISO
  -> Signal dom Bool
  -- ^ Slave select (SS)
  -> Signal dom (BitVector n)
  -- ^ Data to send from slave to master
  --
  -- Input is latched the moment slave select goes low
  -> ( BiSignalOut 'Floating dom 1
     , Signal dom Bool
     , Signal dom (Maybe (BitVector n)))
  -- ^ Parts of the tuple:
  --
  -- 1. The "out" part of the inout port of the MISO; used only for simulation.
  --
  -- 2. (Maybe) the word send by the master
spiSlaveLatticeSBIO mode latchSPI =
  spiSlave (SPISlaveConfig mode latchSPI sbioX)
 where
  sbioX bin en dout = bout
   where
    (bout,_,_) = sbio 0b101001 bin (pure 0) dout (pure undefined) en
