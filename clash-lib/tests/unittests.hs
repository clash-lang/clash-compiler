module Main where

import System.Environment (setEnv)
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Clash.Tests.Core.Scoped.Alpha
import qualified Clash.Tests.Core.Scoped.Binary
import qualified Clash.Tests.Core.Scoped.Builder
import qualified Clash.Tests.Core.Scoped.DataCon
import qualified Clash.Tests.Core.Scoped.EqSolver
import qualified Clash.Tests.Core.Scoped.Evaluator
import qualified Clash.Tests.Core.Scoped.Evaluator.Primitives
import qualified Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Class.BitPack.Internal
import qualified Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.BitVector
import qualified Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.Index
import qualified Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.RTree
import qualified Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector
import qualified Clash.Tests.Core.Scoped.Evaluator.Primitives.GHC.Prim
import qualified Clash.Tests.Core.Scoped.FreeVars
import qualified Clash.Tests.Core.Scoped.HasType
import qualified Clash.Tests.Core.Scoped.NameMap
import qualified Clash.Tests.Core.Scoped.Pretty
import qualified Clash.Tests.Core.Scoped.Scope
import qualified Clash.Tests.Core.Scoped.Subst
import qualified Clash.Tests.Core.Scoped.Term
import qualified Clash.Tests.Core.Scoped.TermLiteral
import qualified Clash.Tests.Core.Scoped.TermInfo
import qualified Clash.Tests.Core.Scoped.TyCon
import qualified Clash.Tests.Core.Scoped.Type
import qualified Clash.Tests.Core.Scoped.TypeEnv
import qualified Clash.Tests.Core.Scoped.TysPrim
import qualified Clash.Tests.Core.Scoped.Util
import qualified Clash.Tests.Driver.Manifest
import qualified Clash.Tests.Netlist.Id
import qualified Clash.Tests.Netlist.Scoped
import qualified Clash.Tests.Netlist.Scoped.BlackBox
import qualified Clash.Tests.Netlist.Util
import qualified Clash.Tests.Primitives
import qualified Clash.Tests.Normalize
import qualified Clash.Tests.Normalize.Scoped.Transformations.ANF
import qualified Clash.Tests.Normalize.Scoped.Transformations.Case
import qualified Clash.Tests.Normalize.Scoped.Transformations.Cast
import qualified Clash.Tests.Normalize.Scoped.Transformations.DEC
import qualified Clash.Tests.Normalize.Scoped.Transformations.EtaExpand
import qualified Clash.Tests.Normalize.Scoped.Transformations.Inline
import qualified Clash.Tests.Normalize.Scoped.Transformations.Letrec
import qualified Clash.Tests.Normalize.Scoped.PrimitiveReductions
import qualified Clash.Tests.Normalize.Scoped.Transformations.MultiPrim
import qualified Clash.Tests.Normalize.Scoped.Transformations.Reduce
import qualified Clash.Tests.Normalize.Scoped.Transformations.SeparateArgs
import qualified Clash.Tests.Normalize.Scoped.Transformations.Specialize
import qualified Clash.Tests.Normalize.Scoped.Transformations.XOptimize
import qualified Clash.Tests.Normalize.Scoped.Util
import qualified Clash.Tests.Rewrite.Scoped.Combinators
import qualified Clash.Tests.Rewrite.Scoped.History
import qualified Clash.Tests.Rewrite.Scoped.StrategyDSL
import qualified Clash.Tests.Rewrite.Scoped.Util
import qualified Clash.Tests.Rewrite.Scoped.Whnf
import qualified Clash.Tests.Rewrite.Scoped.WhnfContext
import qualified Clash.Tests.Rewrite.Scoped.WorkFree
import qualified Clash.Tests.Util.Interpolate
import qualified Clash.Tests.Warning
import qualified Test.Clash.Rewrite
import qualified Test.Clash.Scoped
import qualified Test.Clash.Scoped.PrintTests

-- AFAIK there's no good way to override the default, so we just detect the
-- default value and change it.
setDefaultQuickCheckTests :: QuickCheckTests -> QuickCheckTests
setDefaultQuickCheckTests (QuickCheckTests 100) = 10000
setDefaultQuickCheckTests opt = opt

tests :: TestTree
tests = testGroup "Unittests"
  [ Clash.Tests.Core.Scoped.Alpha.tests
  , Clash.Tests.Core.Scoped.Binary.tests
  , Clash.Tests.Core.Scoped.Builder.tests
  , Clash.Tests.Core.Scoped.DataCon.tests
  , Clash.Tests.Core.Scoped.EqSolver.tests
  , Clash.Tests.Core.Scoped.Evaluator.tests
  , Clash.Tests.Core.Scoped.Evaluator.Primitives.tests
  , Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Class.BitPack.Internal.tests
  , Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.BitVector.tests
  , Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.Index.tests
  , Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.RTree.tests
  , Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector.tests
  , Clash.Tests.Core.Scoped.Evaluator.Primitives.GHC.Prim.tests
  , Clash.Tests.Core.Scoped.FreeVars.tests
  , Clash.Tests.Core.Scoped.HasType.tests
  , Clash.Tests.Core.Scoped.NameMap.tests
  , Clash.Tests.Core.Scoped.Pretty.tests
  , Clash.Tests.Core.Scoped.Scope.tests
  , Clash.Tests.Core.Scoped.Subst.tests
  , Clash.Tests.Core.Scoped.Term.tests
  , Clash.Tests.Core.Scoped.TermLiteral.tests
  , Clash.Tests.Core.Scoped.TermInfo.tests
  , Clash.Tests.Core.Scoped.TyCon.tests
  , Clash.Tests.Core.Scoped.Type.tests
  , Clash.Tests.Core.Scoped.TypeEnv.tests
  , Clash.Tests.Core.Scoped.TysPrim.tests
  , Clash.Tests.Core.Scoped.Util.tests
  , Clash.Tests.Driver.Manifest.tests
  , Clash.Tests.Netlist.Id.tests
  , Clash.Tests.Netlist.Scoped.tests
  , Clash.Tests.Netlist.Scoped.BlackBox.tests
  , Clash.Tests.Netlist.Util.tests
  , Clash.Tests.Primitives.tests
  , Clash.Tests.Normalize.tests
  , Clash.Tests.Normalize.Scoped.Transformations.ANF.tests
  , Clash.Tests.Normalize.Scoped.Transformations.Case.tests
  , Clash.Tests.Normalize.Scoped.Transformations.Cast.tests
  , Clash.Tests.Normalize.Scoped.Transformations.DEC.tests
  , Clash.Tests.Normalize.Scoped.Transformations.EtaExpand.tests
  , Clash.Tests.Normalize.Scoped.Transformations.Inline.tests
  , Clash.Tests.Normalize.Scoped.Transformations.Letrec.tests
  , Clash.Tests.Normalize.Scoped.PrimitiveReductions.tests
  , Clash.Tests.Normalize.Scoped.Transformations.MultiPrim.tests
  , Clash.Tests.Normalize.Scoped.Transformations.Reduce.tests
  , Clash.Tests.Normalize.Scoped.Transformations.SeparateArgs.tests
  , Clash.Tests.Normalize.Scoped.Transformations.Specialize.tests
  , Clash.Tests.Normalize.Scoped.Transformations.XOptimize.tests
  , Clash.Tests.Normalize.Scoped.Util.tests
  , Clash.Tests.Rewrite.Scoped.Combinators.tests
  , Clash.Tests.Rewrite.Scoped.History.tests
  , Clash.Tests.Rewrite.Scoped.StrategyDSL.tests
  , Clash.Tests.Rewrite.Scoped.Util.tests
  , Clash.Tests.Rewrite.Scoped.Whnf.tests
  , Clash.Tests.Rewrite.Scoped.WhnfContext.tests
  , Clash.Tests.Rewrite.Scoped.WorkFree.tests
  , Clash.Tests.Util.Interpolate.tests
  , Clash.Tests.Warning.tests
  , Test.Clash.Rewrite.tests
  , Test.Clash.Scoped.testScopeTests
  , Test.Clash.Scoped.PrintTests.tests
  ]

main :: IO ()
main = do
  -- The tests compare printed terms with what the pretty printer prints with
  -- these settings, which otherwise come from the environment (see
  -- 'Clash.Core.Pretty.PrettyOptions' and 'Clash.Pretty.defaultPprWidth')
  mapM_ (uncurry setEnv)
    [ ("CLASH_PPR_UNIQUES", "False"), ("CLASH_PPR_QUALIFIERS", "False")
    , ("CLASH_PPR_TICKS", "False"), ("CLASH_PPR_WIDTH", "200") ]
  defaultMain
    $ adjustOption setDefaultQuickCheckTests
    $ tests
