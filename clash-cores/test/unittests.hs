{-|
  Copyright   :  (C) 2019, Foamspace corp
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Test driver
-}
module Main where

import Prelude
import Test.Tasty

import qualified Test.Cores.SPI as SPI
import qualified Test.Cores.SPI.MultiSlave as Mul
import qualified Test.Cores.UART as UART

tests :: TestTree
tests = testGroup "Unittests" [SPI.tests, Mul.tests, UART.tests]

main :: IO ()
main = defaultMain tests
