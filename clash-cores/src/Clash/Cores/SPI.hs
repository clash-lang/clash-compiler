{-|
  Copyright   :  (C) 2019, Foamspace corp
                     2022, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  SPI master and slave core
-}
module Clash.Cores.SPI
  ( SPIMode(..)
    -- * SPI master
  , SpiMasterIn(..)
  , SpiMasterOut(..)
  , spiMaster
  , spiMaster1
    -- * SPI slave
  , SpiSlaveIn(..)
  , SpiSlaveOut(..)
  , SPISlaveConfig(..)
  , spiSlave
  , spiSlave1
    -- ** Vendor configured SPI slaves
  , spiSlaveLatticeSBIO
  , spiSlaveLatticeBB
  ) where

import Data.Maybe (fromMaybe, isJust)
import Test.QuickCheck as QC

import Clash.Prelude
import Clash.Sized.Internal.BitVector

import Clash.Cores.LatticeSemi.ICE40.IO
import Clash.Cores.LatticeSemi.ECP5.IO
import Clash.Class.HasDomain

-- | Input signals of an SPI subordinate.
data SpiSlaveIn (ds :: BiSignalDefault) (dom :: Domain) (misoLanes :: Nat) (mosiLanes :: Nat) =
  SpiSlaveIn {
    spiSlaveMosi :: "MOSI" ::: Signal dom (BitVector mosiLanes),
      -- ^ Master-out, slave-in
    spiSlaveMisoIn :: "MISO" ::: BiSignalIn ds dom misoLanes,
      -- ^ Master-in, slave-out
    spiSlaveSck :: "SCK" ::: Signal dom Bit,
      -- ^ Serial Clock
    spiSlaveSs :: "SS" ::: Signal dom Bit
      -- ^ Slave select
  }

type instance TryDomain t (SpiSlaveIn ds dom misoLanes mosiLanes) = 'Found dom

-- | Output signals of an SPI subordinate.
data SpiSlaveOut (ds :: BiSignalDefault) (dom :: Domain) (misoLanes :: Nat) (mosiLanes :: Nat) =
  SpiSlaveOut {
    spiSlaveMisoOut :: "MISO" ::: BiSignalOut ds dom misoLanes
      -- ^ Master-in, slave-out
  }

type instance TryDomain t (SpiSlaveOut ds dom misoLanes mosiLanes) = 'Found dom

-- | Output signals of an SPI master.
data SpiMasterOut (dom :: Domain) (misoLanes :: Nat) (mosiLanes :: Nat) =
  SpiMasterOut {
    spiMasterMosi :: "MOSI" ::: Signal dom (BitVector mosiLanes),
      -- ^ Master-out, slave-in
    spiMasterSck :: "SCK" ::: Signal dom Bit,
      -- ^ Serial Clock
    spiMasterSs :: "SS" ::: Signal dom Bit
      -- ^ Slave select
  }

type instance TryDomain t (SpiMasterOut dom misoLanes mosiLanes) = 'Found dom

-- | Input signals of an SPI master.
data SpiMasterIn (dom :: Domain) (misoLanes :: Nat) (mosiLanes :: Nat) =
  SpiMasterIn {
    spiMasterMiso :: "MISO" ::: Signal dom (BitVector misoLanes)
      -- ^ Master-in, slave-out
  }

type instance TryDomain t (SpiMasterIn dom misoLanes mosiLanes) = 'Found dom

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

data SPISlaveConfig ds dom (misoW :: Nat) (mosiW :: Nat)
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
      :: BiSignalIn ds dom misoW
      -> Signal dom Bool
      -> Signal dom (BitVector misoW)
      -> BiSignalOut ds dom misoW
  -- ^ Tri-state buffer: first argument is the inout pin, second
  -- argument is the output enable, third argument is the value to
  -- output when the enable is high
  }

-- | SPI capture and shift logic that is shared between slave and master
spiCommon
  :: forall n dom inW outW
   . ( HiddenClockResetEnable dom
     , KnownNat inW
     , KnownNat outW
     , KnownNat n
     , 1 <= n )
  => SPIMode
  -> Signal dom Bool
  -- ^ Slave select
  -> Signal dom (BitVector inW)
  -- ^ Slave: MOSI; Master: MISO
  -> Signal dom Bool
  -- ^ SCK
  -> Signal dom (Vec outW (BitVector n))
  -> ( Signal dom (BitVector outW) -- Slave: MISO; Master: MOSI
     , Signal dom Bool             -- Acknowledge start of transfer
     , Signal dom (Maybe (Vec inW (BitVector n)))
     )
spiCommon mode ssI msI sckI dinI =
  mooreB go cvt ( 0 :: Index n      -- cntR
                , False             -- cntOldR
                , undefined         -- sckOldR
                , deepErrorX "no initial dataInR"
                , deepErrorX "no initial dataOutR"
                , False             -- ackR
                , False             -- doneR
                )
                (ssI,msI,sckI,dinI)
 where
  cvt (_,_,_,dataInQ,dataOutQ,ackQ,doneQ) =
    ( v2bv $ map head dataOutQ
    , ackQ
    , if doneQ
         then Just (map v2bv dataInQ)
         else Nothing
    )

  go :: (Index n, Bool, Bool, Vec inW (Vec n Bit), Vec outW (Vec n Bit), Bool, Bool)
     -> (Bool, BitVector inW, Bool, Vec outW (BitVector n))
     -> (Index n, Bool, Bool, Vec inW (Vec n Bit), Vec outW (Vec n Bit), Bool, Bool)
  go (cntQ,cntOldQ,sckOldQ,dataInQ,dataOutQ,_,_) (ss,ms,sck,din) =
    (cntD,cntOldD,sck,dataInD,dataOutD,ackD,doneD)
   where
    cntD
      | ss        = 0
      | sampleSck = if cntQ == maxBound then 0 else cntQ + 1
      | otherwise = cntQ

    dataInD :: Vec inW (Vec n Bit)
    dataInD
      | ss        = unpack undefined#
      | sampleSck = zipWith (\d m -> tail @(n-1) d :< m) dataInQ (bv2v ms)
      | otherwise = dataInQ

    dataOutD :: Vec outW (Vec n Bit)
    dataOutD
      | ss || (sampleOnTrailing mode && sampleSck && cntQ == maxBound) = fmap bv2v din
      | shiftSck  = if sampleOnTrailing mode && cntQ == 0
                    then dataOutQ
                    else map (\d -> tail @(n-1) d :< unpack undefined#) dataOutQ
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
  :: forall n ds dom misoW mosiW
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     , KnownNat misoW
     , KnownNat mosiW )
  => SPISlaveConfig ds dom misoW mosiW
  -- ^ Configure SPI mode and tri-state buffer
  -> SpiSlaveIn ds dom misoW mosiW
  -- ^ SPI interface
  -> Signal dom (Vec misoW (BitVector n))
  -- ^ Data to send from slave to master.
  --
  -- Input is latched the moment slave select goes low
  -> ( SpiSlaveOut ds dom misoW mosiW
     , Signal dom Bool
     , Signal dom (Maybe (Vec mosiW (BitVector n))) )
  -- ^ Parts of the tuple:
  --
  -- 1. The "out" part of the inout port of the MISO; used only for simulation.
  --
  -- 2. the acknowledgement for the data sent from the master to the slave.
  --
  -- 2. (Maybe) the word sent by the master
spiSlave (SPISlaveConfig mode latch buf) (SpiSlaveIn mosi bin sclk ss) din =
  let ssL   = if latch then delay undefined ss   else ss
      mosiL = if latch then delay undefined mosi else mosi
      sclkL = if latch then delay undefined sclk else sclk
      (miso, ack, dout) = spiCommon mode (bitToBool <$> ssL) mosiL (bitToBool <$> sclkL) din
      bout = buf bin (not . bitToBool <$> ssL) miso
  in  (SpiSlaveOut bout, ack, dout)

spiSlave1
  :: forall n ds dom
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n )
  => SPISlaveConfig ds dom 1 1
  -- ^ Configure SPI mode and tri-state buffer
  -> SpiSlaveIn ds dom 1 1
  -- ^ SPI interface
  -> Signal dom (BitVector n)
  -- ^ Data to send from slave to master.
  --
  -- Input is latched the moment slave select goes low
  -> ( SpiSlaveOut ds dom 1 1
     , Signal dom Bool
     , Signal dom (Maybe (BitVector n)) )
  -- ^ Parts of the tuple:
  --
  -- 1. The "out" part of the inout port of the MISO; used only for simulation.
  --
  -- 2. the acknowledgement for the data sent from the master to the slave.
  --
  -- 2. (Maybe) the word sent by the master
spiSlave1 config spiIn dout =
    let (spiOut, ack, din) = spiSlave config spiIn (singleton <$> dout)
    in (spiOut, ack, fmap head <$> din)

-- | SPI master configurable in the SPI mode and clock divider
--
-- Adds latch to MISO line if the (half period) clock divider is
-- set to 2 or higher.
spiMaster
  :: forall n halfPeriod waitTime dom misoW mosiW
   . ( HiddenClockResetEnable dom
     , KnownNat misoW
     , KnownNat mosiW
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
  -> Signal dom (Maybe (Vec mosiW (BitVector n)))
  -- ^ Data to send from master to slave, transmission starts when receiving
  -- /Just/ a value
  -> SpiMasterIn dom misoW mosiW
  -> ( SpiMasterOut dom misoW mosiW
     , Signal dom Bool -- Busy
     , Signal dom Bool -- Acknowledge
     , Signal dom (Maybe (Vec misoW (BitVector n))) -- Data: Slave -> Master
     )
  -- ^ Parts of the tuple:
  --
  -- 1. Serial Clock (SCLK)
  -- 2. Master Output Slave Input (MOSI)
  -- 3. Slave select (SS)
  -- 4. Busy signal indicating that a transmission is in progress, new words on
  --    the data line will be ignored when /True/
  -- 5. (Maybe) the word send from the slave to the master
spiMaster mode fN fW din (SpiMasterIn miso) =
  let (mosi, ack, dout)   = spiCommon mode ssL misoL sclkL
                        (fromMaybe (repeat undefined#) <$> din)
      latch = snatToInteger fN /= 1
      ssL   = if latch then delay undefined ss   else ss
      misoL = if latch then delay undefined miso else miso
      sclkL = if latch then delay undefined sclk else sclk
      (ss, sclk, busy) = spiGen mode fN fW din
  in  (SpiMasterOut mosi (boolToBit <$> sclk) (boolToBit <$> ss), busy, ack, dout)

-- | SPI master with single-bit MISO and MOSI width.
spiMaster1
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
  -> SpiMasterIn dom 1 1
  -> ( SpiMasterOut dom 1 1
     , Signal dom Bool -- Busy
     , Signal dom Bool -- Acknowledge
     , Signal dom (Maybe (BitVector n)) -- Data: Slave -> Master
     )
spiMaster1 mode halfPeriod waitTime dout spiIn =
    let (spiOut, busy, ack, din) =
            spiMaster mode halfPeriod waitTime (fmap singleton <$> dout) spiIn
    in (spiOut, busy, ack, fmap head <$> din)

-- | Generate slave select and SCK
spiGen
  :: forall n halfPeriod waitTime dom outW
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , KnownNat outW
     , 1 <= n
     , 1 <= halfPeriod
     , 1 <= waitTime )
  => SPIMode
  -> SNat halfPeriod
  -> SNat waitTime
  -> Signal dom (Maybe (Vec outW (BitVector n)))
  -> ( Signal dom Bool
     , Signal dom Bool
     , Signal dom Bool
     )
spiGen mode SNat SNat =
  unbundle . moore go cvt (0 :: Index (2*n),False,Idle @halfPeriod @waitTime)
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
        | n == maxBound -> if cntQ == maxBound then 0 else cntQ+1
        | otherwise -> cntQ
      _ -> 0

    sckD = case stQ of
      Transfer n | n == maxBound -> not sckQ
      _ -> sckQ

data SPIMasterState halfPeriod waitTime
  = Idle
  | Wait (Index waitTime)
  | Transfer (Index halfPeriod)
  | Finish
  deriving (Eq, Generic, NFDataX)

-- | SPI slave configurable SPI mode, using the SB_IO tri-state buffer
-- found on Lattice ICE40 Semiconductor FPGAs
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
  -> SpiSlaveIn 'Floating dom 1 1
  -> Signal dom (BitVector n)
  -- ^ Data to send from slave to master
  --
  -- Input is latched the moment slave select goes low
  -> ( SpiSlaveOut 'Floating dom 1 1
     , Signal dom Bool
     , Signal dom (Maybe (BitVector n)))
  -- ^ Parts of the tuple:
  --
  -- 1. The "out" part of the inout port of the MISO; used only for simulation.
  --
  -- 2. (Maybe) the word send by the master
spiSlaveLatticeSBIO mode latchSPI =
  spiSlave1 (SPISlaveConfig mode latchSPI sbioX)
 where
  sbioX bin en dout = bout
   where
    (bout,_,_) = sbio 0b101001 bin (pure 0) (head . bv2v <$> dout) (pure undefined) en


-- | SPI slave configurable SPI mode, using the BB tri-state buffer
-- found on Lattice ECP5 Semiconductor FPGAs
spiSlaveLatticeBB
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
  -> SpiSlaveIn 'Floating dom 1 1
  -> Signal dom (BitVector n)
  -- ^ Data to send from slave to master
  --
  -- Input is latched the moment slave select goes low
  -> ( SpiSlaveOut 'Floating dom 1 1
     , Signal dom Bool
     , Signal dom (Maybe (BitVector n)))
  -- ^ Parts of the tuple:
  --
  -- 1. The "out" part of the inout port of the MISO; used only for simulation.
  --
  -- 2. (Maybe) the word send by the master
spiSlaveLatticeBB mode latchSPI =
  spiSlave1 (SPISlaveConfig mode latchSPI bbX)
 where
    bbX bin en dout = bout
      where
        (bout,_) = bidirectionalBuffer (toEnable en) bin (head . bv2v <$> dout)
