{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.EtaExpand": the tests of
  the transformations of Clash Core, run on the ports through
  'Test.Clash.Scoped.ViaFoil.viaFoil', and agreement of the ports with the
  transformations of Clash Core.

  The regression tests of 'Clash.Normalize.Transformations.EtaExpand.etaExpansionTL'
  are about lambdas moved out of a let-expression that capture, or are
  captured by, its bindings. In scoped Clash Core, the moved lambdas get new
  binders in the scope of the let-expression, so that bug can't be written
  there. 'etaExpandSyn' never fired while capturing the corpus of the
  differential harness, so the tests construct its inputs.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.EtaExpand (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User))
import Clash.Core.Term
  (Bind (..), CoreContext (..), NameMod (..), Term (..), TickInfo (..))
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (mkVarSet)
import qualified Clash.Normalize.Scoped.Transformations.EtaExpand as Scoped
import Clash.Normalize.Types (NormRewrite, NormalizeState)
import Clash.Rewrite.Scoped (ScopedTransform (..))
import Test.Clash.Scoped.ViaFoil (viaFoil)
import Clash.Rewrite.StrategyDSL.TH (asScopedRewriteQ)
import Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), TransformContext (..))
import Clash.Util.Supply (newSupply)

import Clash.Tests.Normalize.Transformations.EtaExpand
  (EtaRewrites (..), currentRewrites, etaTests, fGlobal)
import Test.Clash.Rewrite
  ( assertFires, globalId, inScopeOf, intFunTy, intId, intLit, localId
  , parseToTermQQ, parseTyConTy )
import Test.Clash.Scoped (assertRewritesAgree)

-- | The ports, on terms of Clash Core: through
-- 'Test.Clash.Scoped.ViaFoil.viaFoil'
scopedRewrites :: EtaRewrites
scopedRewrites = EtaRewrites
  { erEtaExpansionTL = viaFoil "etaTL" (ScopedTransform Scoped.etaExpansionTL)
  , erEtaExpandSyn = (viaFoil "etaExpandSyn" $(asScopedRewriteQ Scoped.etaExpandSyn))
  }

-- | Assert that a transformation and its port agree on a term, in an
-- environment with the given top entities
agreeIn
  :: Bool
  -- ^ Whether they fire
  -> [Id]
  -- ^ Top entities
  -> (EtaRewrites -> NormRewrite)
  -> TransformContext
  -> Term
  -> Assertion
agreeIn fires tops rw ctx tm = do
  supply <- newSupply
  let env = def{_topEntities = mkVarSet tops}
      st = def{_uniqSupply = supply} :: RewriteState NormalizeState
  assertFires fires env st ctx (rw currentRewrites) tm
  assertRewritesAgree env st ctx (rw currentRewrites) (rw scopedRewrites) tm

-- * Variables

x, y, z :: Id
x = intId User "x" 31
y = intId User "y" 32
z = intId User "z" 33

-- | Global functions @g :: Int -> Int@ and @k :: Int -> Int -> Int@
g, k :: Id
g = globalId User "g" 41 (intFunTy 1)
k = globalId User "k" 42 (intFunTy 2)

-- | A local function @h :: Int -> Int@
h :: Id
h = localId User "h" 43 (intFunTy 1)

srcTick :: TickInfo
srcTick = NameMod PrefixName (parseTyConTy "Pfx")

-- * etaExpansionTL

etaTLTests :: TestTree
etaTLTests = testGroup "etaExpansionTL"
  [ testCase "eta-expands a function" $
      agreeTL [] (Var k)
  , testCase "goes under lambdas" $
      agreeTL [] (Lam x (App (Var k) (Var x)))
  , testCase "moves lambdas out of a let" $
      agreeTL [] [parseToTermQQ|
        let
          i_1 :: Int
          i_1 = 3
        in
          \(y_2 :: Int) -> \(z_3 :: Int) -> i_1
      |]
  , testCase "moves lambdas out of nested lets" $
      agreeTL [x] $
        Let (NonRec y (Var x))
          (Lam z (Letrec [(x, App (Var g) (Var y))] (Lam y (Var x))))
  , testCase "eta-expands the body of a let" $
      agreeTL [] (Letrec [(y, intLit 3), (z, App (Var g) (Var y))] (App (Var k) (Var z)))
  , testCase "the body of a let is a local function" $
      agreeTL [h] (Let (NonRec y (intLit 3)) (Var h))
  , testCase "doesn't fire on a value" $
      noFireTL [x] (App (Var g) (Var x))
  ]
 where
  agreeTL fvs = agreeIn True [] erEtaExpansionTL (TransformContext (inScopeOf fvs) [])
  noFireTL fvs = agreeIn False [] erEtaExpansionTL (TransformContext (inScopeOf fvs) [])

-- * etaExpandSyn

etaExpandSynTests :: TestTree
etaExpandSynTests = testGroup "etaExpandSyn"
  [ testCase "eta-expands a top entity" $
      agreeSyn [] [] (Var fGlobal)
  , testCase "eta-expands a partially applied top entity" $
      agreeSyn [x] [] (App (Var k) (Var x))
  , testCase "looks through ticks" $
      agreeSyn [x] [] (Tick srcTick (App (Var k) (Var x)))
  , testCase "doesn't eta-expand the function of an application" $
      noFireSyn [] [AppFun] (Var k)
  , testCase "doesn't eta-expand under ticks in the function of an application" $
      noFireSyn [] [TickC srcTick, AppFun] (Var k)
  , testCase "doesn't eta-expand a fully applied top entity" $
      noFireSyn [x, y] [] (App (App (Var k) (Var x)) (Var y))
  , testCase "doesn't eta-expand a function that isn't a top entity" $
      noFireSyn [] [] (Var g)
  ]
 where
  agreeSyn fvs frames =
    agreeIn True [fGlobal, k] erEtaExpandSyn (TransformContext (inScopeOf fvs) frames)
  noFireSyn fvs frames =
    agreeIn False [fGlobal, k] erEtaExpandSyn (TransformContext (inScopeOf fvs) frames)

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.EtaExpand"
  [ testGroup "Regression tests" (etaTests scopedRewrites)
  , etaTLTests
  , etaExpandSynTests
  ]
