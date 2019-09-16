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
          (\_ i -> i)
          (\b -> if b then Nothing else Just val)
          True

testMasterSlave
  :: 1 <= m
  => SPIMode
  -- ^ SPI Mode
  -> Bool
  -- ^ Whether the SPI slave should latch SPI signals
  -> SNat m
  -- ^ Half-period of the clock divider for the SPI master
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
testMasterSlave spiMode latchSPI divHalf mVal sVal duration =
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
      clk rst enableGen spiMode divHalf masterIn (readFromBiSignal miso)

  clk = systemClockGen
  rst = systemResetGen

tests :: TestTree
tests =
  testGroup "SPI"
  [ testCase "Mode0, Divider:8, Slave latch: True"
      (testMasterSlave SPIMode0 True d4 0b0110011101 0b0110010101 (3*84) @?=
       (([0b0110011101],3),([0b0110010101],3)))
  , testCase "Mode1, Divider:8, Slave latch: True"
      (testMasterSlave SPIMode1 True d4 0b0110011101 0b0110010101 (3*84) @?=
       (([0b0110011101],3),([0b0110010101],3)))
  , testCase "Mode2, Divider:8, Slave latch: True"
      (testMasterSlave SPIMode2 True d4 0b0110011101 0b0110010101 (3*84) @?=
       (([0b0110011101],3),([0b0110010101],3)))
  , testCase "Mode3, Divider:8, Slave latch: True"
      (testMasterSlave SPIMode3 True d4 0b0110011101 0b0110010101 (3*84) @?=
       (([0b0110011101],3),([0b0110010101],3)))
  , testCase "Mode0, Divider:2, Slave latch: False"
      (testMasterSlave SPIMode0 False d1 0b0110011101 0b0110010101 (3*25) @?=
       (([0b0110011101],3),([0b0110010101],3)))
  , testCase "Mode1, Divider:2, Slave latch: False"
      (testMasterSlave SPIMode1 False d1 0b0110011101 0b0110010101 (3*25) @?=
       (([0b0110011101],3),([0b0110010101],3)))
  , testCase "Mode2, Divider:2, Slave latch: False"
      (testMasterSlave SPIMode2 False d1 0b0110011101 0b0110010101 (3*25) @?=
       (([0b0110011101],3),([0b0110010101],3)))
  , testCase "Mode3, Divider:2, Slave latch: False"
      (testMasterSlave SPIMode3 False d1 0b0110011101 0b0110010101 (3*25) @?=
       (([0b0110011101],3),([0b0110010101],3)))
  ]
