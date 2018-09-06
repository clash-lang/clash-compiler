module Main where

import Test.Tasty

import qualified Clash.Tests.DerivingDataRepr

tests :: TestTree
tests = testGroup "Unittests"
  [ Clash.Tests.DerivingDataRepr.tests
  ]

main :: IO ()
main = defaultMain tests
