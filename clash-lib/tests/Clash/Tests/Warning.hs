module Clash.Tests.Warning (tests) where

import Data.List (nub)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Warning

allWarnings :: [ClashWarning]
allWarnings = [minBound .. maxBound]

tests :: TestTree
tests =
  testGroup
    "Warning"
    [ testCase "warning names round-trip through parseWarningName" $
        map (parseWarningName . warningName) allWarnings
          @?= map Just allWarnings
    , testCase "warning names are unique" $
        let names = map warningName allWarnings
         in nub names @?= names
    , testCase "warning names start with \"clash-\"" $
        filter (\w -> take 6 (warningName w) /= "clash-") allWarnings @?= []
    ]
