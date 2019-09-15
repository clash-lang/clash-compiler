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
