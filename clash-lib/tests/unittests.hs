module Main where

import Test.Tasty

import qualified Clash.Tests.Core.FreeVars
import qualified Clash.Tests.Core.Subst
import qualified Clash.Tests.Util.Interpolate
import qualified Clash.Tests.Scaffold

tests :: TestTree
tests = testGroup "Unittests"
  [ Clash.Tests.Core.FreeVars.tests
  , Clash.Tests.Core.Subst.tests
  , Clash.Tests.Util.Interpolate.tests
  , Clash.Tests.Scaffold.tests
  ]

main :: IO ()
main = defaultMain tests
