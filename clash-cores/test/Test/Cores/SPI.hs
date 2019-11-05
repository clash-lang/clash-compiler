{-|
  Copyright   :  (C) 2019, Foamspace corp
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for SPI master and slave cores
-}
module Test.Cores.SPI where

import qualified Data.List as L
import Data.Maybe
import qualified Prelude as P

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E

import Clash.Cores.SPI

masterInBP
  :: KnownDomain dom
  => BitVector n
  -> Clock dom
  -> Reset dom
  -> Signal dom Bool
  -> Signal dom (Maybe (BitVector n))
masterInBP val clk rst =
  E.moore clk rst enableGen
    (flip const)
    (\b -> if b then Nothing else Just val)
    True

slaveAddressRotate
  :: forall n dom
   . (KnownDomain dom, KnownNat n, 1 <= n)
  => Clock dom
  -> Reset dom
  -> (Signal dom Bool, Signal dom Bool)
  -> Vec n (Signal dom Bool)
slaveAddressRotate clk rst =
  E.mealyB clk rst enableGen
    (\(cntQ,bQ) (ss,b) ->
        let bF = bQ && not b
            cntD | bF = if cntQ == maxBound then 0 else cntQ + 1
                 | otherwise = cntQ

            oH = map (ss ||) (unpack (complement (bin2onehot cntQ)))
        in  ((cntD,b),oH))
    (0 :: Index n,False)

bin2onehot
  :: (KnownNat n, 1 <= n)
  => Index n
  -> BitVector n
bin2onehot = setBit 0 . fromEnum

testMasterSlave
  :: (1 <= halfPeriod, 1 <= waitTime)
  => SPIMode
  -- ^ SPI Mode
  -> Bool
  -- ^ Whether the SPI slave should latch SPI signals
  -> SNat halfPeriod
  -- ^ Half-period of the clock divider for the SPI master
  -> SNat waitTime
  -- ^ (core clock) cycles between de-assertion of slave-select and start of
  -- the SPI clock
  -> BitVector 10
  -- ^ Value master sends to slave
  -> BitVector 10
  -- ^ Value slave sends to master
  -> Int
  -- ^ Sample duration
  -> (([BitVector 10],Int),([BitVector 10],Int))
  -- ^
  --
  -- 1.1 Different slave outputs captured, should be equal to master value
  -- 1.2 Number of slave outputs captured
  -- 2.1 Different master outputs captured, should be equal to slave value
  -- 2.2 Number of master outputs captured
testMasterSlave spiMode latchSPI divHalf wait mVal sVal duration =
   let s = sampleN duration (bundle (slaveOut,masterOut))
       (slaveOutS,masterOutS) = P.unzip s
       ss0 = catMaybes slaveOutS
       ms0 = catMaybes masterOutS
   in  ((L.nub ss0,P.length ss0),(L.nub ms0,P.length ms0))
 where
  slaveIn = pure sVal
  (misoZ,slaveOut) =
    exposeClockResetEnable spiSlaveLatticeSBIO
      clk rst enableGen spiMode latchSPI sclk mosi miso ss slaveIn
  miso = veryUnsafeToBiSignalIn misoZ

  masterIn = masterInBP mVal clk rst bp

  (sclk,mosi,ss,bp,masterOut) =
    exposeClockResetEnable spiMaster
      clk rst enableGen spiMode divHalf wait masterIn (readFromBiSignal miso)

  clk = systemClockGen
  rst = systemResetGen

testMasterSlaveMultiWord
  :: (1 <= halfPeriod, 1 <= waitTime)
  => SPIMode
  -- ^ SPI Mode
  -> Bool
  -- ^ Whether the SPI slave should latch SPI signals
  -> SNat halfPeriod
  -- ^ Half-period of the clock divider for the SPI master
  -> SNat waitTime
  -- ^ (core clock) cycles between de-assertion of slave-select and start of
  -- the SPI clock
  -> BitVector 16
  -- ^ Value master sends to slave
  -> BitVector 8
  -- ^ Value slave sends to master
  -> Int
  -- ^ Sample duration
  -> ([([BitVector 8],Int)],([BitVector 16],Int))
  -- ^
  --
  -- 1.1 Different slave outputs captured, should be equal to master value
  -- 1.2 Number of slave outputs captured
  -- 2.1 Different master outputs captured, should be equal to slave value
  -- 2.2 Number of master outputs captured
testMasterSlaveMultiWord spiMode latchSPI divHalf wait mVal sVal duration =
   let s = sampleN duration (bundle (slaveOut,masterOut))
       (slaveOutS,masterOutS) = P.unzip s
       ss0 = L.group (L.sort (catMaybes slaveOutS))
       ms0 = catMaybes masterOutS
   in  (L.map (\x -> (L.nub x,P.length x)) ss0,(L.nub ms0,P.length ms0))
 where
  slaveIn = pure sVal
  (misoZ,slaveOut) =
    exposeClockResetEnable spiSlaveLatticeSBIO
      clk rst enableGen spiMode latchSPI sclk mosi miso ss slaveIn
  miso = veryUnsafeToBiSignalIn misoZ

  masterIn = masterInBP mVal clk rst bp

  (sclk,mosi,ss,bp,masterOut) =
    exposeClockResetEnable spiMaster
      clk rst enableGen spiMode divHalf wait masterIn (readFromBiSignal miso)

  clk = systemClockGen
  rst = systemResetGen

testMasterMultiSlave
  :: (1 <= halfPeriod, 1 <= waitTime)
  => SPIMode
  -- ^ SPI Mode
  -> Bool
  -- ^ Whether the SPI slave should latch SPI signals
  -> SNat halfPeriod
  -- ^ Half-period of the clock divider for the SPI master
  -> SNat waitTime
  -- ^ (core clock) cycles between de-assertion of slave-select and start of
  -- the SPI clock
  -> BitVector 10
  -- ^ Value master sends to slave
  -> BitVector 10
  -- ^ Value slave sends to master
  -> Int
  -- ^ Sample duration
  -> (([BitVector 10],Int)
     ,([BitVector 10],Int)
     ,([BitVector 10],Int)
     ,([BitVector 10],Int))
  -- ^
  --
  -- 1.1 Different slave 0 outputs captured, should be equal to master value
  -- 1.2 Number of slave 0 outputs captured
  -- ...
  -- N.1 Different master outputs captured, should be equal to slave value
  -- N.2 Number of master outputs captured
testMasterMultiSlave spiMode latchSPI divHalf wait mVal sVal duration =
   let s = sampleN duration (bundle (slaveOut0,slaveOut1,slaveOut2,masterOut))
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
  (misoZ0,slaveOut0) =
    exposeClockResetEnable spiSlaveLatticeSBIO
      clk rst enableGen spiMode latchSPI sclk mosi miso ss0 slaveIn

  (misoZ1,slaveOut1) =
    exposeClockResetEnable spiSlaveLatticeSBIO
      clk rst enableGen spiMode latchSPI sclk mosi miso ss1 slaveIn

  (misoZ2,slaveOut2) =
    exposeClockResetEnable spiSlaveLatticeSBIO
      clk rst enableGen spiMode latchSPI sclk mosi miso ss2 slaveIn

  miso = veryUnsafeToBiSignalIn
         (mergeBiSignalOuts (misoZ2 :> misoZ1 :> misoZ0 :> Nil))

  masterIn = masterInBP mVal clk rst bp

  (ss2 :> ss1 :> ss0 :> Nil) = slaveAddressRotate @3 clk rst (ss,bp)

  (sclk,mosi,ss,bp,masterOut) =
    exposeClockResetEnable spiMaster
      clk rst enableGen spiMode divHalf wait masterIn (readFromBiSignal miso)

  clk = systemClockGen
  rst = systemResetGen

singleSlave :: [TestTree]
singleSlave =
  [testLatch, testNoLatch] <*> [SPIMode0, SPIMode1, SPIMode2, SPIMode3]
 where
  testLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterSlave spi True d4 d1 0b0110011101 0b0110010101 (3 * 84)
        @?= (([0b0110011101], 3), ([0b0110010101], 3))

  testNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterSlave spi False d1 d1 0b0110011101 0b0110010101 (3 * 25)
        @?= (([0b0110011101], 3), ([0b0110010101], 3))

multipleSlaves :: [TestTree]
multipleSlaves =
  [testLatch, testNoLatch] <*> [SPIMode0, SPIMode1, SPIMode2, SPIMode3]
 where
  testLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterMultiSlave spi True d4 d1 0b0110011101 0b0110010101 (3 * 84)
        @?= (([0b0110011101],1),([0b0110011101],1),([0b0110011101],1),([0b0110010101],3))

  testNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterMultiSlave spi False d1 d1 0b0110011101 0b0110010101 (3 * 25)
        @?= (([0b0110011101],1),([0b0110011101],1),([0b0110011101],1),([0b0110010101],3))

singleSlaveMultipleWords :: [TestTree]
singleSlaveMultipleWords =
  [testLatch, testNoLatch] <*> [SPIMode0, SPIMode1, SPIMode2, SPIMode3]
 where
  testLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterSlaveMultiWord spi True d4 d1 0b0101010100001111 0b10010101 (5 * 84)
        @?= ([([0b00001111], 3), ([0b01010101], 3)], ([0b01001010110010101], 3))

  testNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterSlaveMultiWord spi False d1 d1 0b0101010100001111 0b0110010101 (5 * 25)
        @?= ([([0b00001111],3),([0b01010101],3)],([0b01001010110010101],3))

singleSlaveWait :: [TestTree]
singleSlaveWait =
  [testLatch, testNoLatch] <*> [SPIMode0, SPIMode1, SPIMode2, SPIMode3]
 where
  testLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterSlave spi True d4 d3 0b0110011101 0b0110010101 (3 * 87)
        @?= (([0b0110011101],3),([0b0110010101],3))

  testNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterSlave spi False d1 d3 0b0110011101 0b0110010101 (3 * 27)
        @?= (([0b0110011101],3),([0b0110010101],3))

multipleSlavesWait :: [TestTree]
multipleSlavesWait =
  [testLatch, testNoLatch] <*> [SPIMode0, SPIMode1, SPIMode2, SPIMode3]
 where
  testLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterMultiSlave spi True d4 d3 0b0110011101 0b0110010101 (3 * 87)
        @?= (([0b0110011101],1),([0b0110011101],1),([0b0110011101],1),([0b0110010101],3))

  testNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterMultiSlave spi False d1 d3 0b0110011101 0b0110010101 (3 * 27)
        @?= (([0b0110011101],1),([0b0110011101],1),([0b0110011101],1),([0b0110010101],3))

singleSlaveMultipleWordsWait :: [TestTree]
singleSlaveMultipleWordsWait =
  [testLatch, testNoLatch] <*> [SPIMode0, SPIMode1, SPIMode2, SPIMode3]
 where
  testLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterSlaveMultiWord spi True d4 d3 0b0101010100001111 0b10010101 (5 * 87)
        @?= ([([0b00001111],3),([0b01010101],3)],([0b01001010110010101],3))

  testNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterSlaveMultiWord spi False d1 d3 0b0101010100001111 0b10010101 (5 * 27)
        @?= ([([0b00001111],3),([0b01010101],3)],([0b01001010110010101],3))

tests :: TestTree
tests =
  testGroup "SPI"
    [ testGroup "Single slave" singleSlave
    , testGroup "Multiple slaves (3)" multipleSlaves
    , testGroup "Single slave, multiple words" singleSlaveMultipleWords
    , testGroup "Single slave, 3 cycle wait" singleSlaveWait
    , testGroup "Multiple slaves (3), 3 cycle wait" multipleSlavesWait
    , testGroup "Single slave, multiple words, 3 cycle wait" singleSlaveMultipleWordsWait
    ]

