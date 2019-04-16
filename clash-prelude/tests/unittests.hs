module Main where

import Test.Tasty

import qualified Clash.Tests.DerivingDataRepr
import qualified Clash.Tests.Undefined

tests :: TestTree
tests = testGroup "Unittests"
  [ Clash.Tests.DerivingDataRepr.tests
  , Clash.Tests.Undefined.tests
  ]

main :: IO ()
main = defaultMain tests
