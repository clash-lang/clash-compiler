module Main where

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Clash.Tests.Core.AlphaEquivalence
import qualified Clash.Tests.Core.FreeVars
import qualified Clash.Tests.Core.HasType
import qualified Clash.Tests.Core.Lint
import qualified Clash.Tests.Core.Scoped.Alpha
import qualified Clash.Tests.Core.Scoped.Convert
import qualified Clash.Tests.Core.Scoped.DataCon
import qualified Clash.Tests.Core.Scoped.EqSolver
import qualified Clash.Tests.Core.Scoped.FreeVars
import qualified Clash.Tests.Core.Scoped.HasType
import qualified Clash.Tests.Core.Scoped.NameMap
import qualified Clash.Tests.Core.Scoped.Scope
import qualified Clash.Tests.Core.Scoped.Subst
import qualified Clash.Tests.Core.Scoped.Term
import qualified Clash.Tests.Core.Scoped.TermInfo
import qualified Clash.Tests.Core.Scoped.TyCon
import qualified Clash.Tests.Core.Scoped.Type
import qualified Clash.Tests.Core.Scoped.TysPrim
import qualified Clash.Tests.Core.Scoped.Util
import qualified Clash.Tests.Core.StructuralEquivalence
import qualified Clash.Tests.Core.Subst
import qualified Clash.Tests.Core.TermLiteral
import qualified Clash.Tests.Core.Util
import qualified Clash.Tests.Driver.Manifest
import qualified Clash.Tests.Netlist
import qualified Clash.Tests.Netlist.Id
import qualified Clash.Tests.Netlist.Util
import qualified Clash.Tests.Normalize
import qualified Clash.Tests.Normalize.PrimitiveReductions
import qualified Clash.Tests.Normalize.Transformations
import qualified Clash.Tests.Normalize.Transformations.ANF
import qualified Clash.Tests.Normalize.Transformations.Case
import qualified Clash.Tests.Normalize.Transformations.Cast
import qualified Clash.Tests.Normalize.Transformations.DEC
import qualified Clash.Tests.Normalize.Transformations.EtaExpand
import qualified Clash.Tests.Normalize.Transformations.Inline
import qualified Clash.Tests.Normalize.Transformations.MultiPrim
import qualified Clash.Tests.Normalize.Transformations.Letrec
import qualified Clash.Tests.Normalize.Transformations.SeparateArgs
import qualified Clash.Tests.Normalize.Transformations.Specialize
import qualified Clash.Tests.Normalize.Transformations.XOptimize
import qualified Clash.Tests.Normalize.Util
import qualified Clash.Tests.Rewrite.Combinators
import qualified Clash.Tests.Rewrite.Scoped.WorkFree
import qualified Clash.Tests.Rewrite.StrategyDSL
import qualified Clash.Tests.Rewrite.Util
import qualified Clash.Tests.Rewrite.WorkFree
import qualified Clash.Tests.Util.Interpolate
import qualified Clash.Tests.Warning
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
  , Clash.Tests.Core.HasType.tests
  , Clash.Tests.Core.Lint.tests
  , Clash.Tests.Core.Scoped.Alpha.tests
  , Clash.Tests.Core.Scoped.Convert.tests
  , Clash.Tests.Core.Scoped.DataCon.tests
  , Clash.Tests.Core.Scoped.EqSolver.tests
  , Clash.Tests.Core.Scoped.FreeVars.tests
  , Clash.Tests.Core.Scoped.HasType.tests
  , Clash.Tests.Core.Scoped.NameMap.tests
  , Clash.Tests.Core.Scoped.Scope.tests
  , Clash.Tests.Core.Scoped.Subst.tests
  , Clash.Tests.Core.Scoped.Term.tests
  , Clash.Tests.Core.Scoped.TermInfo.tests
  , Clash.Tests.Core.Scoped.TyCon.tests
  , Clash.Tests.Core.Scoped.Type.tests
  , Clash.Tests.Core.Scoped.TysPrim.tests
  , Clash.Tests.Core.Scoped.Util.tests
  , Clash.Tests.Core.StructuralEquivalence.tests
  , Clash.Tests.Core.Subst.tests
  , Clash.Tests.Core.TermLiteral.tests
  , Clash.Tests.Core.Util.tests
  , Clash.Tests.Driver.Manifest.tests
  , Clash.Tests.Netlist.tests
  , Clash.Tests.Netlist.Id.tests
  , Clash.Tests.Netlist.Util.tests
  , Clash.Tests.Normalize.tests
  , Clash.Tests.Normalize.PrimitiveReductions.tests
  , Clash.Tests.Normalize.Transformations.tests
  , Clash.Tests.Normalize.Transformations.ANF.tests
  , Clash.Tests.Normalize.Transformations.Case.tests
  , Clash.Tests.Normalize.Transformations.Cast.tests
  , Clash.Tests.Normalize.Transformations.DEC.tests
  , Clash.Tests.Normalize.Transformations.EtaExpand.tests
  , Clash.Tests.Normalize.Transformations.Inline.tests
  , Clash.Tests.Normalize.Transformations.MultiPrim.tests
  , Clash.Tests.Normalize.Transformations.Letrec.tests
  , Clash.Tests.Normalize.Transformations.SeparateArgs.tests
  , Clash.Tests.Normalize.Transformations.Specialize.tests
  , Clash.Tests.Normalize.Transformations.XOptimize.tests
  , Clash.Tests.Normalize.Util.tests
  , Clash.Tests.Rewrite.Combinators.tests
  , Clash.Tests.Rewrite.Scoped.WorkFree.tests
  , Clash.Tests.Rewrite.StrategyDSL.tests
  , Clash.Tests.Rewrite.Util.tests
  , Clash.Tests.Rewrite.WorkFree.tests
  , Clash.Tests.Util.Interpolate.tests
  , Clash.Tests.Warning.tests
  , Test.Clash.Rewrite.tests
  ]

main :: IO ()
main =
    defaultMain
  $ adjustOption setDefaultQuickCheckTests
  $ tests
