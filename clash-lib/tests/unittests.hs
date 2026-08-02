module Main where

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Clash.Tests.Core.AlphaEquivalence
import qualified Clash.Tests.Core.FreeVars
import qualified Clash.Tests.Core.Subst
import qualified Clash.Tests.Core.TermLiteral
import qualified Clash.Tests.Driver.Manifest
import qualified Clash.Tests.Netlist.Id
import qualified Clash.Tests.Normalize.Transformations
import qualified Clash.Tests.Rewrite.StrategyDSL
import qualified Clash.Tests.Util.Interpolate
import qualified Test.Clash.Rewrite

-- AFAIK there's no good way to override the default, so we just detect the
-- default value and change it.
setDefaultQuickCheckTests :: QuickCheckTests -> QuickCheckTests
setDefaultQuickCheckTests (QuickCheckTests 100) = 10000
setDefaultQuickCheckTests opt = opt

tests :: TestTree
tests = testGroup "Unittests"
  [ Clash.Tests.Core.AlphaEquivalence.tests
  , Clash.Tests.Core.FreeVars.tests
  , Clash.Tests.Core.Subst.tests
  , Clash.Tests.Core.TermLiteral.tests
  , Clash.Tests.Driver.Manifest.tests
  , Clash.Tests.Netlist.Id.tests
  , Clash.Tests.Normalize.Transformations.tests
  , Clash.Tests.Rewrite.StrategyDSL.tests
  , Clash.Tests.Util.Interpolate.tests
  , Test.Clash.Rewrite.tests
  ]

main :: IO ()
main =
    defaultMain
  $ adjustOption setDefaultQuickCheckTests
  $ tests
