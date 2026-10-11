{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the differential tests at captured sites,
  "Test.Clash.Differential.Sites"
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Differential.Sites (tests) where

import Data.Default (def)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import Clash.Core.VarEnv (mkInScopeSet, mkVarSet)
import qualified Clash.Normalize.Scoped.Transformations.Letrec as S
import qualified Clash.Normalize.Transformations.Letrec as Letrec
import Clash.Rewrite.Capture (Capture (..))
import Clash.Rewrite.Scoped (ScopedTransform (..))
import Clash.Rewrite.StrategyDSL (onLet, toTransformation)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ, asScopedRewriteQ)
import Clash.Rewrite.Types (TransformContext (..))

import Test.Clash.Differential
import Test.Clash.Differential.Sites
import Test.Clash.Rewrite (globalId, intId, intLit, intTy, mkBindingMap)

tests :: TestTree
tests = testGroup "Clash.Tests.Differential.Sites"
  [ testCase "deadCode reproduces its captured result, and its port agrees" $ do
      cap <- mkCapture "deadCode" f site deadLet (C.Var a)
      SiteOutcome replay scoped _ <- compareAt deadCodeCmp cap
      assertReproduced replay
      case scoped of
        Just Equal -> pure ()
        _ -> assertFailure "expected the scoped side to be equal"

  , testCase "a wrong captured result is not reproduced" $ do
      cap <- mkCapture "deadCode" f site deadLet (C.Var b)
      SiteOutcome replay _ _ <- compareAt deadCodeCmp cap
      case replay of
        NotReproduced (Mismatch {}) -> pure ()
        _ -> assertFailure "expected the current side not to reproduce it"

  , testCase "a transformation that doesn't fire at the site" $ do
      cap <- mkCapture "deadCode" f site (C.Var a) (C.Var a)
      SiteOutcome replay _ _ <- compareAt deadCodeCmp cap
      case replay of
        NotFired _ -> pure ()
        _ -> assertFailure "expected the current side not to fire"

  , testCase "a scoped side that doesn't fire is different" $ do
      cap <- mkCapture "deadCode" f site deadLet (C.Var a)
      let cmp = deadCodeCmp {scScoped = Just (ScopedTransform (\_ e -> pure e))}
      SiteOutcome replay scoped _ <- compareAt cmp cap
      assertReproduced replay
      case scoped of
        Just (Different (Mismatch {})) -> pure ()
        _ -> assertFailure "expected the scoped side to be different"

  , testCase "the context is trimmed to what the term uses" $ do
      cap <- mkCapture "deadCode" f site deadLet (C.Var a)
      case capContext cap of
        [C.LetBody [(a', _)], C.LamBody x'] -> do
          a' @?= a
          x' @?= x
        _ -> assertFailure "expected the binding of a only"
  ]
 where
  f = globalId C.User "f" 100 intTy
  x = intId C.User "x" 1
  a = intId C.User "a" 2
  b = intId C.User "b" 3
  y = intId C.User "y" 4

  -- \x -> let a = x; b = 2 in <site>
  site = TransformContext
    (mkInScopeSet (mkVarSet [x, a, b]))
    [C.LetBody [(a, C.Var x), (b, intLit 2)], C.LamBody x]

  -- let y = 3 in a
  deadLet = C.Let (C.NonRec y (intLit 3)) (C.Var a)

  deadCodeCmp = SiteComparison
    { scName = "deadCode"
    , scDescription = ""
    , scTransformations = ["deadCode"]
    , scCurrent = $(asRewriteQ (toTransformation "deadCode" (onLet 'Letrec.deadCodeWorker)))
    , scScoped = Just $(asScopedRewriteQ S.deadCode)
    , scKnownDifferences = []
    }

  compareAt cmp cap = do
    let bs = mkBindingMap [(f, C.Lam x (intLit 0))]
    c <- mkCorpus def (emptyFinalState def bs) []
    compareSite 10000000 cmp c cap

  assertReproduced = \case
    Reproduced -> pure ()
    _ -> assertFailure "expected the current side to reproduce the captured result"
