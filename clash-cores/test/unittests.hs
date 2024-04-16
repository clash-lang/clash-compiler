{-|
  Copyright   :  (C) 2019, Foamspace corp
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Test driver
-}
module Main where

import Prelude
import Test.Tasty

import qualified Test.Cores.Crc
import qualified Test.Cores.LineCoding8b10b
import qualified Test.Cores.SPI
import qualified Test.Cores.SPI.MultiSlave
import qualified Test.Cores.UART
import qualified Test.Cores.Xilinx.BlockRam
import qualified Test.Cores.Xilinx.DcFifo
import qualified Test.Cores.Xilinx.DnaPortE2
import qualified Test.Cores.Xilinx.Ethernet.Gmii

tests :: TestTree
tests = testGroup "Unittests"
  [ Test.Cores.Crc.tests
  , Test.Cores.LineCoding8b10b.tests
  , Test.Cores.SPI.tests
  , Test.Cores.SPI.MultiSlave.tests
  , Test.Cores.UART.tests
  , Test.Cores.Xilinx.BlockRam.tests
  , Test.Cores.Xilinx.DcFifo.tests
  , Test.Cores.Xilinx.DnaPortE2.tests
  , Test.Cores.Xilinx.Ethernet.Gmii.tests
  ]

main :: IO ()
main = defaultMain tests
