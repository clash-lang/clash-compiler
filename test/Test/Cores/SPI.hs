module Test.Cores.SPI where

import Data.Maybe
import qualified Prelude as P

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E

import Clash.Cores.SPI

testMode :: SPIMode
testMode = SPIMode2

masterInBP
  :: KnownDomain dom
  => BitVector 8
  -> Clock dom
  -> Reset dom
  -> Signal dom Bool
  -> Signal dom (Maybe (BitVector 8))
masterInBP val clk rst =
  E.moore clk rst enableGen
          (\_ i -> i)
          (\b -> if b then Nothing else Just val)
          True

testMasterSlave :: Signal System (Maybe (BitVector 8), Maybe (BitVector 8))
testMasterSlave = bundle (slaveOut,masterOut)
 where
  slaveIn = pure (0b01100111 :: BitVector 8)
  (misoZ,slaveOut) =
    exposeClockResetEnable spiSlaveLatticeSBIO
      clk rst enableGen testMode True sclk mosi miso ss slaveIn
  miso = veryUnsafeToBiSignalIn misoZ

  masterIn = masterInBP (0b01100111 :: BitVector 8) clk rst bp

  (sclk,mosi,ss,bp,masterOut) =
    exposeClockResetEnable spiMaster
      clk rst enableGen testMode d4 masterIn (readFromBiSignal miso)

  clk = systemClockGen
  rst = systemResetGen

masterX =
  let s = sampleN 100 testMasterSlave
      (ss,ms) = P.unzip s
      ss0 = catMaybes ss
      ms0 = catMaybes ms
  in  ((ss0,P.length ss0),(ms0,P.length ms0))

tests :: TestTree
tests =
  testGroup "SPI"
  [ testCase "Mode2" (masterX @?= (([0b01100111],1),([0b01100111],1)))
  ]
