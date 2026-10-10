{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Util"
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Tests.Normalize.Util (tests) where

import GHC.Builtin.Names (eqTyConKey)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.HasFreeVars (freeVarsOf)
import Clash.Core.Name (NameSort (..), mkUnsafeName)
import Clash.Core.Term (Term (..))
import Clash.Core.Type
  (LitTy (..), Type (..), TypeView (..), mkFunTy, mkTyConApp, tyView)
import Clash.Core.TyCon (TyConName)
import Clash.Core.TysPrim (liftedTypeKind)
import Clash.Core.Var (TyVar)
import Clash.Core.VarEnv (nullVarSet)
import Clash.Normalize.Util (substWithTyEq, tvSubstWithTyEq)
import Clash.Unique (fromGhcUnique)

import Test.Clash.Rewrite (assertAlphaEq, localId, showPprU, tyVar)

-- | @a ~ b@, recognized by 'substWithTyEq' and 'tvSubstWithTyEq'
eqTy :: Type -> Type -> Type
eqTy a b = mkTyConApp eqTyConNm [liftedTypeKind, a, b]
 where
  eqTyConNm :: TyConName
  eqTyConNm = mkUnsafeName User "~" (fromGhcUnique eqTyConKey)

systemTy :: Type
systemTy = LitTy (SymTy "System")

dom, a2, c3 :: TyVar
dom = tyVar User "dom" 1
a2 = tyVar User "a" 2
c3 = tyVar User "c" 3

-- | 'substWithTyEq' turns @/\\dom -> \\(eq :: dom ~ System) -> ...@ into
-- @\\(eq :: System ~ System) -> ...@. It used to drop the lambda for @eq@, but
-- left its occurrences alone, making them free.
-- https://github.com/clash-lang/clash-compiler/issues/1058,
-- https://github.com/clash-lang/clash-compiler/pull/1060
substWithTyEqClosed :: Assertion
substWithTyEqClosed =
  assertBool ("free variables in result: " <> showPprU tm1)
    (nullVarSet (freeVarsOf tm1))
 where
  eq = localId User "eq" 2 (eqTy (VarTy dom) systemTy)
  tm0 = TyLam dom (Lam eq (Var eq))
  tm1 = substWithTyEq tm0

-- | 'substWithTyEq' must not let the type it substitutes for @dom@ be captured
-- by a type binder in the body: substituting @[dom := a]@ in
-- @/\\a -> \\(x :: dom) -> x@ must rename the inner @a@. It used to substitute
-- with an empty in-scope set.
-- https://github.com/clash-lang/clash-compiler/pull/1868
substWithTyEqNoCapture :: Assertion
substWithTyEqNoCapture = assertAlphaEq expected (substWithTyEq tm)
 where
  eq = localId User "eq" 3 (eqTy (VarTy dom) (VarTy a2))
  x = localId User "x" 4 (VarTy dom)
  -- /\a -> /\dom -> \(eq :: dom ~ a) -> /\a -> \(x :: dom) -> x
  tm = TyLam a2 (TyLam dom (Lam eq (TyLam a2 (Lam x (Var x)))))

  eq' = localId User "eq" 3 (eqTy (VarTy a2) (VarTy a2))
  x' = localId User "x" 4 (VarTy a2)
  -- /\a -> \(eq :: a ~ a) -> /\c -> \(x :: a) -> x
  expected = TyLam a2 (Lam eq' (TyLam c3 (Lam x' (Var x'))))

-- | 'substWithTyEq' must keep the order of the type and term lambdas it
-- doesn't remove. It used to collect them in two separate lists, putting all
-- term lambdas outside of all type lambdas. Here that would move @x :: a@ out
-- of the scope of @a@.
-- https://github.com/clash-lang/clash-compiler/pull/1868
substWithTyEqOrder :: Assertion
substWithTyEqOrder = assertAlphaEq expected (substWithTyEq tm)
 where
  eq = localId User "eq" 3 (eqTy (VarTy dom) systemTy)
  x = localId User "x" 4 (VarTy a2)
  y = localId User "y" 5 (VarTy a2)
  -- /\a -> \(x :: a) -> /\dom -> \(eq :: dom ~ System) -> \(y :: a) -> y
  tm = TyLam a2 (Lam x (TyLam dom (Lam eq (Lam y (Var y)))))

  eq' = localId User "eq" 3 (eqTy systemTy systemTy)
  -- /\a -> \(x :: a) -> \(eq :: System ~ System) -> \(y :: a) -> y
  expected = TyLam a2 (Lam x (Lam eq' (Lam y (Var y))))

-- | The type level equivalent of 'substWithTyEqNoCapture': in
-- @forall a dom. (dom ~ a) -> forall a. dom -> a@, substituting
-- @[dom := a]@ must rename the inner @a@, giving @forall c. a -> c@ after the
-- equality constraint. It used to substitute with an empty in-scope set.
-- https://github.com/clash-lang/clash-compiler/pull/1868
tvSubstWithTyEqNoCapture :: Assertion
tvSubstWithTyEqNoCapture =
  case tvSubstWithTyEq ty of
    ForAllTy a (tyView -> FunTy _eq rest) | a == a2 ->
      assertAlphaEq (ForAllTy c3 (VarTy a2 `mkFunTy` VarTy c3)) rest
    ty1 -> assertFailure ("unexpected result: " <> showPprU ty1)
 where
  ty =
    ForAllTy a2 $ ForAllTy dom $
      eqTy (VarTy dom) (VarTy a2) `mkFunTy`
        ForAllTy a2 (VarTy dom `mkFunTy` VarTy a2)

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Util"
    [ testCase "substWithTyEq doesn't introduce free variables (#1060)"
        substWithTyEqClosed
    , testCase "substWithTyEq avoids capture (#1868)"
        substWithTyEqNoCapture
    , testCase "substWithTyEq keeps the order of TyLams and Lams (#1868)"
        substWithTyEqOrder
    , testCase "tvSubstWithTyEq avoids capture (#1868)"
        tvSubstWithTyEqNoCapture
    ]
