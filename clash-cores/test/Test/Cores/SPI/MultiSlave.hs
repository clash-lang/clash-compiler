module Test.Cores.SPI.MultiSlave where

import qualified Prelude as P (length)
import qualified Data.List as L (nub, unzip4)
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.HUnit

import           Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import           Clash.Cores.SPI

import           Test.Cores.Internal.SampleSPI
import           Test.Cores.Internal.Signals

-- TODO Ideally there should only be one generic test driver which allows
-- tests to specify the number of slaves used in the simulation. This would
-- also be a step towards allowing multi slave tests to run with an arbitrary
-- number of slaves (using QuickCheck).
--
slaveAddressRotate
  :: forall n dom
   . (KnownNat n, 1 <= n)
  => Clock dom
  -> Reset dom
  -> (Signal dom Bool, Signal dom Bool)
  -> Vec n (Signal dom Bool)
slaveAddressRotate clk rst =
  E.mealyB clk rst enableGen
    (\(cntQ, bQ) (ss, b) ->
        let bF = bQ && not b
            cntD | bF = if cntQ == maxBound then 0 else cntQ + 1
                 | otherwise = cntQ

            oH = (ss ||) <$> (unpack . complement $ bin2onehot cntQ)
        in  ((cntD, b), oH))
    (0 :: Index n, False)
 where
  bin2onehot = setBit 0 . fromEnum

-- Values from SPI master / slave.
-- This is a pair of distinct readings and the number of readings.
--
type Value n = ([BitVector n], Int)

testMasterMultiSlave
  :: (1 <= halfPeriod, 1 <= waitTime)
  => SimpleTest halfPeriod waitTime 10 10 (Value 10, Value 10, Value 10, Value 10)
  -- ^ Outputs (slave0, slave1, slave2, master)
testMasterMultiSlave divHalf wait mVal sVal mode latch duration =
   let s = sampleN (getDuration duration) (bundle (slaveOut0,slaveOut1,slaveOut2,masterOut))
       (slaveOut0MS,slaveOut1MS,slaveOut2MS,masterOutMS) = L.unzip4 s
       slaveOut0S = catMaybes slaveOut0MS
       slaveOut1S = catMaybes slaveOut1MS
       slaveOut2S = catMaybes slaveOut2MS
       masterOutS = catMaybes masterOutMS
   in  ((L.nub slaveOut0S,P.length slaveOut0S)
       ,(L.nub slaveOut1S,P.length slaveOut1S)
       ,(L.nub slaveOut2S,P.length slaveOut2S)
       ,(L.nub masterOutS,P.length masterOutS))
 where
  slaveIn = pure sVal
  (misoZ0, _, slaveOut0) =
    withClockResetEnable clk rst enableGen
      (spiSlaveLatticeSBIO mode latch sclk mosi miso ss0 slaveIn)

  (misoZ1, _, slaveOut1) =
    withClockResetEnable clk rst enableGen
      (spiSlaveLatticeSBIO mode latch sclk mosi miso ss1 slaveIn)

  (misoZ2, _, slaveOut2) =
    withClockResetEnable clk rst enableGen
      (spiSlaveLatticeSBIO mode latch sclk mosi miso ss2 slaveIn)

  miso = veryUnsafeToBiSignalIn
         (mergeBiSignalOuts (misoZ2 :> misoZ1 :> misoZ0 :> Nil))

  masterIn = masterLawfulSignal clk rst (pure mVal) masterAck bp

  (ss2 `Cons` ss1 `Cons` ss0 `Cons` _) = slaveAddressRotate @3 clk rst (ss,bp)

  (sclk, mosi, ss, bp, masterAck, masterOut) =
    withClockResetEnable clk rst enableGen
      (spiMaster mode divHalf wait masterIn (readFromBiSignal miso))

  clk = systemClockGen
  rst = systemResetGen

tests :: TestTree
tests =
  testGroup "Multiple Slaves" $
    [ threeSlaveLatch
    , threeSlaveNoLatch
    , threeSlavesDelayLatch
    , threeSlavesDelayNoLatch
    ] <*> [SPIMode0, SPIMode1, SPIMode2, SPIMode3]
 where
  threeSlaveLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterMultiSlave d4 d1 0b0110011101 0b0110010101 spi True (3 * 85)
        @?= (([0b0110011101],1),([0b0110011101],1),([0b0110011101],1),([0b0110010101],3))

  threeSlaveNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterMultiSlave d1 d1 0b0110011101 0b0110010101 spi False (3 * 25)
        @?= (([0b0110011101],1),([0b0110011101],1),([0b0110011101],1),([0b0110010101],3))

  threeSlavesDelayLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterMultiSlave d4 d3 0b0110011101 0b0110010101 spi True (3 * 87)
        @?= (([0b0110011101],1),([0b0110011101],1),([0b0110011101],1),([0b0110010101],3))

  threeSlavesDelayNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterMultiSlave d1 d3 0b0110011101 0b0110010101 spi False (3 * 27)
        @?= (([0b0110011101],1),([0b0110011101],1),([0b0110011101],1),([0b0110010101],3))
