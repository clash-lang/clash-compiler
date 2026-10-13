module Main where

import Control.Monad (when)
import Data.Maybe (isNothing)
import System.Environment (lookupEnv, setEnv)
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Clash.Tests.Core.Alpha
import qualified Clash.Tests.Core.Binary
import qualified Clash.Tests.Core.Builder
import qualified Clash.Tests.Core.DataCon
import qualified Clash.Tests.Core.EqSolver
import qualified Clash.Tests.Core.Evaluator
import qualified Clash.Tests.Core.Evaluator.Primitives
import qualified Clash.Tests.Core.Evaluator.Primitives.Clash.Class.BitPack.Internal
import qualified Clash.Tests.Core.Evaluator.Primitives.Clash.Sized.Internal.BitVector
import qualified Clash.Tests.Core.Evaluator.Primitives.Clash.Sized.Internal.Index
import qualified Clash.Tests.Core.Evaluator.Primitives.Clash.Sized.RTree
import qualified Clash.Tests.Core.Evaluator.Primitives.Clash.Sized.Vector
import qualified Clash.Tests.Core.Evaluator.Primitives.GHC.Prim
import qualified Clash.Tests.Core.FreeVars
import qualified Clash.Tests.Core.HasType
import qualified Clash.Tests.Core.NameMap
import qualified Clash.Tests.Core.Pretty
import qualified Clash.Tests.Core.Scope
import qualified Clash.Tests.Core.Subst
import qualified Clash.Tests.Core.Term
import qualified Clash.Tests.Core.TermLiteral
import qualified Clash.Tests.Core.TermInfo
import qualified Clash.Tests.Core.TyCon
import qualified Clash.Tests.Core.Type
import qualified Clash.Tests.Core.TypeEnv
import qualified Clash.Tests.Core.TysPrim
import qualified Clash.Tests.Core.Util
import qualified Clash.Tests.Driver.Manifest
import qualified Clash.Tests.Netlist.Id
import qualified Clash.Tests.Netlist
import qualified Clash.Tests.Netlist.BlackBox
import qualified Clash.Tests.Netlist.Util
import qualified Clash.Tests.Primitives
import qualified Clash.Tests.Normalize
import qualified Clash.Tests.Normalize.Transformations.ANF
import qualified Clash.Tests.Normalize.Transformations.Case
import qualified Clash.Tests.Normalize.Transformations.Cast
import qualified Clash.Tests.Normalize.Transformations.DEC
import qualified Clash.Tests.Normalize.Transformations.EtaExpand
import qualified Clash.Tests.Normalize.Transformations.Inline
import qualified Clash.Tests.Normalize.Transformations.Letrec
import qualified Clash.Tests.Normalize.PrimitiveReductions
import qualified Clash.Tests.Normalize.Transformations.MultiPrim
import qualified Clash.Tests.Normalize.Transformations.Reduce
import qualified Clash.Tests.Normalize.Transformations.SeparateArgs
import qualified Clash.Tests.Normalize.Transformations.Specialize
import qualified Clash.Tests.Normalize.Transformations.XOptimize
import qualified Clash.Tests.Normalize.Util
import qualified Clash.Tests.Rewrite.Combinators
import qualified Clash.Tests.Rewrite.History
import qualified Clash.Tests.Rewrite.StrategyDSL
import qualified Clash.Tests.Rewrite.Util
import qualified Clash.Tests.Rewrite.Whnf
import qualified Clash.Tests.Rewrite.WhnfContext
import qualified Clash.Tests.Rewrite.WorkFree
import qualified Clash.Tests.Util.Interpolate
import qualified Clash.Tests.Warning
import qualified Test.Clash.Rewrite
import qualified Test.Clash.Core
import qualified Test.Clash.Core.PrintTests

-- AFAIK there's no good way to override the default, so we just detect the
-- default value and change it.
setDefaultQuickCheckTests :: QuickCheckTests -> QuickCheckTests
setDefaultQuickCheckTests (QuickCheckTests 100) = 10000
setDefaultQuickCheckTests opt = opt

tests :: TestTree
tests = testGroup "Unittests"
  [ Clash.Tests.Core.Alpha.tests
  , Clash.Tests.Core.Binary.tests
  , Clash.Tests.Core.Builder.tests
  , Clash.Tests.Core.DataCon.tests
  , Clash.Tests.Core.EqSolver.tests
  , Clash.Tests.Core.Evaluator.tests
  , Clash.Tests.Core.Evaluator.Primitives.tests
  , Clash.Tests.Core.Evaluator.Primitives.Clash.Class.BitPack.Internal.tests
  , Clash.Tests.Core.Evaluator.Primitives.Clash.Sized.Internal.BitVector.tests
  , Clash.Tests.Core.Evaluator.Primitives.Clash.Sized.Internal.Index.tests
  , Clash.Tests.Core.Evaluator.Primitives.Clash.Sized.RTree.tests
  , Clash.Tests.Core.Evaluator.Primitives.Clash.Sized.Vector.tests
  , Clash.Tests.Core.Evaluator.Primitives.GHC.Prim.tests
  , Clash.Tests.Core.FreeVars.tests
  , Clash.Tests.Core.HasType.tests
  , Clash.Tests.Core.NameMap.tests
  , Clash.Tests.Core.Pretty.tests
  , Clash.Tests.Core.Scope.tests
  , Clash.Tests.Core.Subst.tests
  , Clash.Tests.Core.Term.tests
  , Clash.Tests.Core.TermLiteral.tests
  , Clash.Tests.Core.TermInfo.tests
  , Clash.Tests.Core.TyCon.tests
  , Clash.Tests.Core.Type.tests
  , Clash.Tests.Core.TypeEnv.tests
  , Clash.Tests.Core.TysPrim.tests
  , Clash.Tests.Core.Util.tests
  , Clash.Tests.Driver.Manifest.tests
  , Clash.Tests.Netlist.Id.tests
  , Clash.Tests.Netlist.tests
  , Clash.Tests.Netlist.BlackBox.tests
  , Clash.Tests.Netlist.Util.tests
  , Clash.Tests.Primitives.tests
  , Clash.Tests.Normalize.tests
  , Clash.Tests.Normalize.Transformations.ANF.tests
  , Clash.Tests.Normalize.Transformations.Case.tests
  , Clash.Tests.Normalize.Transformations.Cast.tests
  , Clash.Tests.Normalize.Transformations.DEC.tests
  , Clash.Tests.Normalize.Transformations.EtaExpand.tests
  , Clash.Tests.Normalize.Transformations.Inline.tests
  , Clash.Tests.Normalize.Transformations.Letrec.tests
  , Clash.Tests.Normalize.PrimitiveReductions.tests
  , Clash.Tests.Normalize.Transformations.MultiPrim.tests
  , Clash.Tests.Normalize.Transformations.Reduce.tests
  , Clash.Tests.Normalize.Transformations.SeparateArgs.tests
  , Clash.Tests.Normalize.Transformations.Specialize.tests
  , Clash.Tests.Normalize.Transformations.XOptimize.tests
  , Clash.Tests.Normalize.Util.tests
  , Clash.Tests.Rewrite.Combinators.tests
  , Clash.Tests.Rewrite.History.tests
  , Clash.Tests.Rewrite.StrategyDSL.tests
  , Clash.Tests.Rewrite.Util.tests
  , Clash.Tests.Rewrite.Whnf.tests
  , Clash.Tests.Rewrite.WhnfContext.tests
  , Clash.Tests.Rewrite.WorkFree.tests
  , Clash.Tests.Util.Interpolate.tests
  , Clash.Tests.Warning.tests
  , Test.Clash.Rewrite.tests
  , Test.Clash.Core.testScopeTests
  , Test.Clash.Core.PrintTests.tests
  ]

main :: IO ()
main = do
  -- The tests compare printed terms with what the pretty printer prints with
  -- these settings, which otherwise come from the environment (see
  -- 'Clash.Core.Pretty.PrettyOptions' and 'Clash.Pretty.defaultPprWidth')
  mapM_ (uncurry setEnv)
    [ ("CLASH_PPR_UNIQUES", "False"), ("CLASH_PPR_QUALIFIERS", "False")
    , ("CLASH_PPR_TICKS", "False"), ("CLASH_PPR_WIDTH", "200") ]
  -- Run the tests one at a time, unless asked otherwise with @-j@ or
  -- @TASTY_NUM_THREADS@. Running them in parallel hangs about one in sixty
  -- runs with GHC 9.12.4, because of a bug in its RTS
  -- (https://gitlab.haskell.org/ghc/ghc/-/issues/26936): when a thread that
  -- evaluates a CAF that other threads wait for meets a thunk that another
  -- thread evaluated too, it wakes the waiting threads without updating the
  -- CAF. Every thread that enters the CAF then spins without allocating, so
  -- the program hangs at full CPU, and neither tasty's timeouts nor SIGTERM
  -- stop it. Here, it happens to the top-level types that the evaluator
  -- tests share. GHC 9.14.2 fixes the bug, and so will GHC 9.12.5.
  numThreads <- lookupEnv "TASTY_NUM_THREADS"
  when (isNothing numThreads) $
    setEnv "TASTY_NUM_THREADS" "1"
  defaultMain
    $ adjustOption setDefaultQuickCheckTests
    $ tests
