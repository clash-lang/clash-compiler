{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.XOptimize": the tests of
  'Clash.Normalize.Transformations.XOptimize.xOptimize' of Clash Core, run on
  the port through 'Test.Clash.Scoped.ViaFoil.viaFoil', and agreement of
  the port with 'xOptimize' of Clash Core.

  The regression tests are about pattern binders that capture the free
  variables of the subject, and the binder of the subject clashing with a
  pattern binder. In scoped Clash Core, the port creates new binders for them
  in the scope of the case-expression, so those bugs can't be written there.
  'xOptimize' only runs with aggressive X-optimization, and never fired while
  capturing the corpus of the differential harness, so the tests construct
  its inputs.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.XOptimize (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (IntLiteral))
import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Pat (..), Term (..))
import Clash.Core.TyCon (TyCon (tyConName), TyConMap)
import Clash.Core.Type (Type (VarTy))
import Clash.Core.Var (Id, TyVar)
import Clash.Core.VarEnv (extendInScopeSet)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (ClashEnv (..), ClashOpts (..))
import qualified Clash.Normalize.Scoped.Transformations.XOptimize as Scoped
import Clash.Normalize.Types (NormRewrite, NormalizeState)
import Clash.Rewrite.StrategyDSL.TH (asScopedRewriteQ)
import Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), TransformContext (..))
import Clash.Util.Supply (newSupply)

import Clash.Tests.Normalize.Transformations.XOptimize
  ( envXOpt, errPrim, fId, mkTDc, tN, tTy, xOptimizeCurrent, xOptimizeTests )
import Test.Clash.Differential (KnownDifference, Outcome (..), primTypeForm)
import Test.Clash.Rewrite
  ( assertFires, globalId, inScopeOf, intFunTy, intId, intTy, justDataCon, localId
  , maybeTy, nothingDataCon, pairDataCon, pairTy, testTyConMap, tyVar
  , withTyConMap )
import Test.Clash.Scoped (assertRewritesAgreeUpTo)
import Test.Clash.Scoped.ViaFoil (viaFoil)

-- | The port, on terms of Clash Core: through
-- 'Test.Clash.Scoped.ViaFoil.viaFoil'
xOptimizeScoped :: NormRewrite
xOptimizeScoped = (viaFoil "xOptimize" $(asScopedRewriteQ Scoped.xOptimize))

-- | 'envXOpt', with the type constructors of 'testTyConMap' too
env :: RewriteEnv
env = withTyConMap tcm envXOpt
 where
  tcm :: TyConMap
  tcm = foldr (\tc -> UniqMap.insert (tyConName tc) tc) testTyConMap
          (UniqMap.elems (envTyConMap (_clashEnv envXOpt)))

-- | Assert that 'xOptimize' and its port agree on a term, with the given
-- variables in scope, up to the given known differences. Gives the outcome.
agreeUpTo :: Bool -> [KnownDifference] -> RewriteEnv -> [Id] -> [TyVar] -> Term -> IO Outcome
agreeUpTo fires kds e ids tvs tm = do
  supply <- newSupply
  let st = def{_uniqSupply = supply} :: RewriteState NormalizeState
      ctx = TransformContext (foldl extendInScopeSet (inScopeOf ids) tvs) []
  assertFires fires e st ctx xOptimizeCurrent tm
  assertRewritesAgreeUpTo kds e st ctx xOptimizeCurrent xOptimizeScoped tm

-- | Assert that 'xOptimize' and its port agree on a term, and whether they
-- fire
agree :: Bool -> [Id] -> [TyVar] -> Term -> Assertion
agree fires ids tvs tm = agreeUpTo fires [] env ids tvs tm >>= \case
  Equal -> pure ()
  KnownDifferent kds -> assertFailure ("known differences: " <> show kds)
  Different _ -> assertFailure "different"

-- * Variables

s, m, t :: Id
s = localId User "s" 30 pairTy
m = localId User "m" 31 (maybeTy intTy)
t = localId User "t" 32 tTy

x, y, z :: Id
x = intId User "x" 33
y = intId User "y" 34
z = intId User "z" 35

-- | Global function @g :: Int -> Int@
g :: Term
g = Var (globalId User "g" 40 (intFunTy 1))

f :: Term
f = Var fId

lit :: Integer -> Pat
lit = LitPat . IntLiteral

-- | A free type variable @a@
a :: TyVar
a = tyVar User "a" 50

agreementTests :: TestTree
agreementTests = testGroup "Agreement"
  [ testCase "no defined alternatives" $
      agree True [x] [] (Case (Var x) intTy [(lit 1, errPrim), (DefaultPat, errPrim)])
  , testCase "no defined alternatives, at an open type" $
      agreeUpTo True [primTypeForm] env [x] [a] (Case (Var x) (VarTy a) [(DefaultPat, errPrim)])
        >>= \case
          KnownDifferent ["prim-type-form"] -> pure ()
          o -> assertFailure ("expected the prim-type-form known difference: " <> showOutcome o)
  , testCase "one defined alternative with a pattern" $
      agree True [s] [] $
        Case (Var s) intTy
          [ (DataPat pairDataCon [] [x, y], App f (Var y))
          , (DefaultPat, errPrim) ]
  , testCase "one defined alternative with a literal" $
      agree True [x] [] (Case (Var x) intTy [(lit 1, App f (Var x)), (DefaultPat, errPrim)])
  , testCase "one defined default alternative" $
      agree True [x] [] (Case (Var x) intTy [(lit 1, errPrim), (DefaultPat, App g (Var x))])
  , testCase "one defined alternative whose pattern binds a type variable" $ do
      let v = localId User "v" 36 (VarTy tN)
      agree True [t] [] $
        Case (Var t) intTy [(DefaultPat, errPrim), (DataPat mkTDc [tN] [v], intLitT 3)]
  , testCase "many defined alternatives, without a default" $
      agree True [m] [] $
        Case (Var m) intTy
          [ (DataPat nothingDataCon [] [], intLitT 3)
          , (DataPat justDataCon [] [z], App f (Var z))
          , (DefaultPat, errPrim) ]
  , testCase "many defined alternatives, with a default" $
      agree True [x] [] $
        Case (Var x) intTy
          [ (lit 1, errPrim), (lit 2, App f (Var x)), (DefaultPat, App g (Var x)) ]
  , testCase "all alternatives defined" $
      agree False [x] [] (Case (Var x) intTy [(lit 1, App f (Var x)), (DefaultPat, App g (Var x))])
  , testCase "doesn't fire without aggressive X-optimization" $ do
      let envNoXOpt = env{_clashEnv = clashEnv{envOpts = (envOpts clashEnv){opt_aggressiveXOpt = False}}}
          clashEnv = _clashEnv env
      _ <- agreeUpTo False [] envNoXOpt [x] [] (Case (Var x) intTy [(DefaultPat, errPrim)])
      pure ()
  ]
 where
  intLitT = Literal . IntLiteral
  showOutcome = \case
    Equal -> "equal"
    KnownDifferent kds -> "known differences " <> show kds
    Different _ -> "different"

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.XOptimize"
  [ testGroup "Regression tests" (xOptimizeTests xOptimizeScoped)
  , agreementTests
  ]
