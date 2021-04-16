{-|
  Copyright   :  (C) 2019, Foamspace corp
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Test driver
-}
module Main where

import Prelude
import Test.Tasty

import qualified Test.Cores.LatticeSemi.ICE40.IO as IO
import qualified Test.Cores.SPI as SPI
import qualified Test.Cores.SPI.MultiSlave as Mul

tests :: TestTree
tests = testGroup "Unittests" [IO.tests, SPI.tests, Mul.tests]

main :: IO ()
main = defaultMain tests

