module Main where

import Control.Monad (when)
import Data.Maybe (isNothing)
import System.Environment (lookupEnv, setEnv)
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Clash.Tests.Core.AlphaEquivalence
import qualified Clash.Tests.Core.FreeVars
import qualified Clash.Tests.Core.StructuralEquivalence
import qualified Clash.Tests.Core.Subst
import qualified Clash.Tests.Core.TermLiteral
import qualified Clash.Tests.Driver.Manifest
import qualified Clash.Tests.Netlist
import qualified Clash.Tests.Netlist.Id
import qualified Clash.Tests.Normalize
import qualified Clash.Tests.Normalize.Transformations
import qualified Clash.Tests.Normalize.Transformations.ANF
import qualified Clash.Tests.Normalize.Transformations.Case
import qualified Clash.Tests.Normalize.Transformations.DEC
import qualified Clash.Tests.Normalize.Transformations.Inline
import qualified Clash.Tests.Normalize.Transformations.Letrec
import qualified Clash.Tests.Normalize.Transformations.Specialize
import qualified Clash.Tests.Normalize.Util
import qualified Clash.Tests.Rewrite.StrategyDSL
import qualified Clash.Tests.Rewrite.Util
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
  , Clash.Tests.Core.StructuralEquivalence.tests
  , Clash.Tests.Core.Subst.tests
  , Clash.Tests.Core.TermLiteral.tests
  , Clash.Tests.Driver.Manifest.tests
  , Clash.Tests.Netlist.tests
  , Clash.Tests.Netlist.Id.tests
  , Clash.Tests.Normalize.tests
  , Clash.Tests.Normalize.Transformations.tests
  , Clash.Tests.Normalize.Transformations.ANF.tests
  , Clash.Tests.Normalize.Transformations.Case.tests
  , Clash.Tests.Normalize.Transformations.DEC.tests
  , Clash.Tests.Normalize.Transformations.Inline.tests
  , Clash.Tests.Normalize.Transformations.Letrec.tests
  , Clash.Tests.Normalize.Transformations.Specialize.tests
  , Clash.Tests.Normalize.Util.tests
  , Clash.Tests.Rewrite.StrategyDSL.tests
  , Clash.Tests.Rewrite.Util.tests
  , Clash.Tests.Util.Interpolate.tests
  , Clash.Tests.Warning.tests
  , Test.Clash.Rewrite.tests
  ]

main :: IO ()
main = do
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
