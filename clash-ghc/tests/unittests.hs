module Main where

import Test.Tasty

import qualified Clash.Tests.GHC.ClashFlags

tests :: TestTree
tests = testGroup "Unittests"
  [ Clash.Tests.GHC.ClashFlags.tests
  ]

main :: IO ()
main = defaultMain tests
