module Main where

import Test.Tasty

import qualified Clash.Tests.Core.FreeVars

tests :: TestTree
tests = testGroup "Unittests"
  [ Clash.Tests.Core.FreeVars.tests
  ]

main :: IO ()
main = defaultMain tests
