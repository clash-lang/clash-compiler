{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.Cast". The tests are
  parameterised over the implementation of the transformations
  ('CastRewrites'), so that they also run on their ports to scoped Clash
  Core, see "Clash.Tests.Normalize.Scoped.Transformations.Cast".
-}

{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.Cast
  ( tests
  , CastRewrites (..)
  , currentRewrites
  , castTests
  ) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Bind (..), Term (..))
import Clash.Normalize.Transformations.Cast
  (caseCastWorker, elimCastCastWorker, letCastWorker, splitCastWorkWorker)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL (onCast, onLet, toTransformation)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)

import Test.Clash.Rewrite
  ( assertAlphaEq, globalId, inScopeOf, intFunTy, intId, intTy
  , runSingleTransformation )

-- | The transformations of "Clash.Normalize.Transformations.Cast"
data CastRewrites = CastRewrites
  { crCaseCast :: NormRewrite
  , crElimCastCast :: NormRewrite
  , crLetCast :: NormRewrite
  , crSplitCastWork :: NormRewrite
  }

-- | The transformations of Clash Core. They are made from their workers: the
-- transformations of "Clash.Normalize.Transformations.Cast" are the ports to
-- scoped Clash Core once the normalizer runs those.
currentRewrites :: CastRewrites
currentRewrites = CastRewrites
  { crCaseCast = $(asRewriteQ (toTransformation "caseCast" (onCast 'caseCastWorker)))
  , crElimCastCast = $(asRewriteQ (toTransformation "elimCastCast" (onCast 'elimCastCastWorker)))
  , crLetCast = $(asRewriteQ (toTransformation "letCast" (onCast 'letCastWorker)))
  , crSplitCastWork = $(asRewriteQ (toTransformation "splitCastWork" (onLet 'splitCastWorkWorker)))
  }

-- | @let x = cast (g x) in x@, with the outer @x@ on the right-hand side.
-- 'splitCastWork' must keep the binding non-recursive:
--
-- @
-- let x' = g x in let x = cast x' in x
-- @
--
-- It used to rebuild it as @letrec x' = g x; x = cast x' in x@, where @g x@
-- refers to the cast itself.
splitCastWorkNonRec :: NormRewrite -> Assertion
splitCastWorkNonRec splitCastWorkR = do
  result <- runSingleTransformation def def (inScopeOf [x]) splitCastWorkR term
  assertAlphaEq expected result
 where
  x = intId User "x" 0
  x' = intId User "x'" 1
  g = Var (globalId User "g" 2 (intFunTy 1))
  term = Let (NonRec x (Cast (App g (Var x)) intTy intTy)) (Var x)
  expected =
    Let (NonRec x' (App g (Var x)))
      (Let (NonRec x (Cast (Var x') intTy intTy)) (Var x))

-- | The tests of the transformations, given their implementation
castTests :: CastRewrites -> [TestTree]
castTests rws =
  [ testCase "splitCastWork keeps a NonRec let non-recursive"
      (splitCastWorkNonRec (crSplitCastWork rws))
  ]

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Transformations.Cast" (castTests currentRewrites)
