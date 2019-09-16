{-|
  Copyright   :  (C) 2019, Foamspace corp
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Test driver
-}
module Main where

import Prelude
import Test.Tasty

import qualified Test.Cores.SPI

tests :: TestTree
tests = testGroup "Unittests"
  [ Test.Cores.SPI.tests
  ]

main :: IO ()
main = defaultMain tests
