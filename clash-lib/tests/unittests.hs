module Main where

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Clash.Tests.Core.FreeVars
import qualified Clash.Tests.Core.Subst
import qualified Clash.Tests.Netlist.Id
import qualified Clash.Tests.Util.Interpolate
import qualified Clash.Tests.Normalize.Transformations

-- AFAIK there's no good way to override the default, so we just detect the
-- default value and change it.
setDefaultQuickCheckTests :: QuickCheckTests -> QuickCheckTests
setDefaultQuickCheckTests (QuickCheckTests 100) = 10000
setDefaultQuickCheckTests opt = opt

tests :: TestTree
tests = testGroup "Unittests"
  [ Clash.Tests.Core.FreeVars.tests
  , Clash.Tests.Core.Subst.tests
  , Clash.Tests.Util.Interpolate.tests
  , Clash.Tests.Netlist.Id.tests
  , Clash.Tests.Normalize.Transformations.tests
  ]

main :: IO ()
main =
    defaultMain
  $ adjustOption setDefaultQuickCheckTests
  $ tests
