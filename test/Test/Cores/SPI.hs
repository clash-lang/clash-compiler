module Test.Cores.SPI where

import Data.Maybe
import qualified Prelude as P

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import Clash.Explicit.Testbench
import Clash.Sized.Internal.BitVector (undefined#)

import Clash.Cores.SPI
import Clash.Cores.LatticeSemi.IO

testMode :: SPIMode
testMode = SPIMode2

spiSlaveLattice
  :: forall dom n
   . (HiddenClockResetEnable dom, 1 <= n, KnownNat n)
  => BiSignalIn 'Floating dom 1
  -> Signal dom Bool
  -> Signal dom Bit
  -> Signal dom Bool
  -> Signal dom (BitVector n)
  -> (BiSignalOut 'Floating dom 1, Signal dom (Maybe (BitVector n)))
spiSlaveLattice =
  spiSlave (SPISlaveConfig testMode True sbioX)
 where
  sbioX bin en dout = bout
   where
    (bout,_,_) = sbio 0b101001 bin (pure 0) dout (pure undefined) en

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
    exposeClockResetEnable spiSlaveLattice
      clk rst enableGen miso ss mosi sck slaveIn
  miso = veryUnsafeToBiSignalIn misoZ

  masterIn = masterInBP (0b01100111 :: BitVector 8) clk rst bp

  (masterOut,bp,ss,mosi,sck) =
    exposeClockResetEnable spiMaster
      clk rst enableGen testMode d4 (readFromBiSignal miso) masterIn

  clk = systemClockGen
  rst = systemResetGen

masterX =
  let s = sampleN 500 testMasterSlave
      (ss,ms) = P.unzip s
      ss0 = catMaybes ss
      ms0 = catMaybes ms
  in  ((ss0,P.length ss0),(ms0,P.length ms0))
