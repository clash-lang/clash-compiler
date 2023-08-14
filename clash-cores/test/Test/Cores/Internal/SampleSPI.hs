{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cores.Internal.SampleSPI
  ( SPISamples(..)
  , Duration(..)
  , CyclingTest
  , SimpleTest
  , sampleCycling
  , sampleSimple
  ) where

import qualified Data.List as List (unzip4)
import           Data.List.NonEmpty (NonEmpty)
import           Test.QuickCheck (Arbitrary(..), suchThat)

import           Clash.Prelude
import           Clash.Cores.SPI

import           Test.Cores.Internal.Signals

-- Generally speaking, test units will be able to determine passing or failing
-- by looking at the values of the master and slave, and the acknowledgement
-- signals sent over time.
--
data SPISamples master slave = SPISamples
  { ssMasterOut :: [Maybe (BitVector master)]
  , ssMasterAck :: [Bool]
  , ssSlaveOut  :: [Maybe (BitVector slave)]
  , ssSlaveAck  :: [Bool]
  } deriving (Eq, Show)

newtype Duration = Duration { getDuration :: Int }
  deriving stock   (Eq, Ord, Show)
  deriving newtype (Num)

instance Arbitrary Duration where
  arbitrary = Duration <$> arbitrary `suchThat` (>= 100)

-- | A cycling test is one where the data sent by SPI masters and slaves
-- changes over time, cycling through a list of values to send for each.
--
type CyclingTest halfPeriod waitTime master slave a =
     SNat halfPeriod
  -- ^ Half-period of the clock divider for the SPI master
  -> SNat waitTime
  -- ^ Core clock cycles between de-assertion of slave-select
  -- and the start of the SPI clock
  -> NonEmpty (BitVector master)
  -- ^ Values master sends to slave
  -> NonEmpty (BitVector slave)
  -- ^ Values slave sends to master
  -> SPIMode
  -- ^ SPI Mode
  -> Bool
  -- ^ Whether the SPI slave should latch signals
  -> Duration
  -- ^ Sample duration
  -> a
  -- ^ Output of test function

sampleCycling
  :: ( KnownNat master, KnownNat slave
     , 1 <= halfPeriod, 1 <= waitTime, 1 <= master, 1 <= slave
     )
  => GenMaster master
  -> GenSlave slave
  -> CyclingTest halfPeriod waitTime master slave (SPISamples master slave)
sampleCycling genM genS divHalf wait mVals sVals mode latch duration =
  (\(mO, mA, sO, sA) -> SPISamples mO mA sO sA) $ List.unzip4 samples
 where
  samples = sampleN (getDuration duration) $ bundle (mOut, mAck, sOut, sAck)
  slaveIn = genS clk rst sVals sAck

  (misoZ, sAck, sOut) =
    withClockResetEnable clk rst enableGen
      (spiSlaveLatticeSBIO mode latch sclk mosi miso ss slaveIn)

  miso = veryUnsafeToBiSignalIn misoZ
  masterIn = genM clk rst mVals mAck bp

  (sclk, mosi, ss, bp, mAck, mOut) =
    withClockResetEnable clk rst enableGen
      (spiMaster mode divHalf wait masterIn (readFromBiSignal miso))

  clk = systemClockGen
  rst = systemResetGen

type SimpleTest halfPeriod waitTime master slave a =
     SNat halfPeriod
  -- ^ Half-period of the clock divider for the SPI master
  -> SNat waitTime
  -- ^ Core clock cycles between de-assertion of slave-select
  -- and the start of the SPI clock
  -> BitVector master
  -- ^ Value master sends to slave
  -> BitVector slave
  -- ^ Value slave sends to master
  -> SPIMode
  -- ^ SPI Mode
  -> Bool
  -- ^ Whether the SPI slave should latch signals
  -> Duration
  -- ^ Sample duration
  -> a
  -- ^ Output of test function (depends on test)

sampleSimple
  :: ( KnownNat master, KnownNat slave
     , 1 <= halfPeriod, 1 <= waitTime, 1 <= master, 1 <= slave
     )
  => GenMaster master
  -> GenSlave slave
  -> SimpleTest halfPeriod waitTime master slave (SPISamples master slave)
sampleSimple genM genS divHalf wait mVal sVal =
  sampleCycling genM genS divHalf wait (pure mVal) (pure sVal)
