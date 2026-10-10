{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the conversion between Clash Core and scoped Clash Core
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.Core.Scoped.Convert (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite
  ( assertAlphaEq, assertErrorContains, assertNoShadowing, assertStructurallyEqual
  , existentialDataCon, existentialTy, existentialTyVar, globalId, intId, intTy
  , localId, parseToTermQQ, tyVar)
import Test.Clash.Scoped (roundTrip, withFreeVarsOf)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Convert"
  [ testCase "round trip is alpha-equivalent" $ do
      let tm = [parseToTermQQ|
            \(f_1 :: Int) -> let { (x_2 :: Int) = f_1; (y_3 :: Int) = x_2 } in
              (\(z_4 :: Int) -> z_4) y_3
          |]
      assertAlphaEq tm (roundTrip tm)

  , testCase "conversion numbers binders depth-first" $ do
      let tm = [parseToTermQQ|
            \(f_7 :: Int) (x_3 :: Int) ->
              f_7 (let (y_5 :: Int) = x_3 in y_5) (\(z_4 :: Int) -> z_4)
          |]
          expected = [parseToTermQQ|
            \(f_1 :: Int) (x_2 :: Int) ->
              f_1 (let (y_3 :: Int) = x_2 in y_3) (\(z_4 :: Int) -> z_4)
          |]
      assertStructurallyEqual expected (roundTrip tm)

  , testCase "conversion numbers binders above the free variables" $ do
      let tm = [parseToTermQQ| \(y_1 :: Int) -> (x_5 :: Int) |]
          expected = [parseToTermQQ| \(y_6 :: Int) -> (x_5 :: Int) |]
      assertStructurallyEqual expected (roundTrip tm)

  , testCase "conversion deshadows" $ do
      let x = intId C.User "x" 1
          tm = C.Lam x (C.Lam x (C.Var x))
          tm1 = roundTrip tm
      assertAlphaEq tm tm1
      assertNoShadowing tm1

  , testCase "existentials scope over field types" $ do
      let v = localId C.User "v" 60 existentialTy
          y = localId C.User "y" 61 (C.VarTy existentialTyVar)
          tm = C.Lam v (C.Case (C.Var v) existentialTy [(C.DataPat existentialDataCon [existentialTyVar] [y], C.Var y)])
          tm1 = roundTrip tm
      assertAlphaEq tm tm1
      case tm1 of
        C.Lam _ (C.Case _ _ [(C.DataPat _ [tv] [y1], _)]) ->
          assertBool "the field's type is the existential" (C.varType y1 == C.VarTy tv)
        _ -> assertFailure "unexpected shape"

    -- A type variable and a term variable can share a unique. The term variable
    -- must not be taken for the type variable.
    -- https://github.com/clash-lang/clash-compiler/issues/1046,
    -- https://github.com/clash-lang/clash-compiler/pull/1048
  , testCase "type and term variables with the same unique stay apart (#1048)" $ do
      let a = tyVar C.User "a" 20
          t = localId C.User "t" 20 (C.VarTy a)
      withConvEnv [a] [t] $ \env ->
        case toClashTerm (fromClashTerm env (C.Var t)) of
          C.Var t1@C.Id {} -> do
            -- The term variable was renamed, as the type variable came first
            assertBool "renamed" (C.varUniq t1 /= 20)
            -- Its type is still the type variable
            assertAlphaEq (C.VarTy a) (C.varType t1)
          tm -> assertFailure ("unexpected: " <> show tm)

    -- 'GenerateBindings' makes local copies of the globals of a recursive
    -- group, which keep the unique of their global
  , testCase "local and global variables with the same unique stay apart" $ do
      let fL = intId C.User "f" 7
          fG = globalId C.User "f" 7 intTy
          tm = C.Letrec [(fL, C.Var fG)] (C.Var fL)
          tm1 = fromClashTermClosed tm
      assertAlphaEq tm (toClashTerm tm1)
      case tm1 of
        Let (Rec (TCons b TNil) [GlobalVar g]) (Var _ _ n) -> do
          globalIdVar g @?= fG
          assertBool "the body refers to the local binder" (tmNameOf b == n)
        _ -> assertFailure "unexpected shape"

  , testCase "errors on local variables that are not in scope" $
      assertErrorContains "variable not in scope"
        (toClashTerm (fromClashTermClosed (C.Var (intId C.User "x" 1))))

  , testCase "errors on type variables that are not in scope" $
      assertErrorContains "type variable not in scope"
        (toClashType (fromClashTypeClosed emptyScope (C.VarTy (tyVar C.User "a" 1))))

    -- 'fromClashTyVars' binds names that the free variables of an extension of
    -- its environment must not get
  , testCase "free variables don't get the names of converted type variables" $ do
      let a = tyVar C.User "a" 100
      fromClashTyVars emptyConvEnv [a] $ \env1 tele -> case tele of
        TCons b TNil -> do
          -- The supply starts just above the watermark of the empty scope
          tyNameRaw (tyNameOf b) @?= 1
          let x = intId C.User "x" 1
          extendConvEnv env1 [] [x] $ \env2 ->
            case fromClashTerm env2 (C.Var x) of
              Var _ _ n -> assertBool "renamed" (tmNameRaw n /= 1)
              _ -> assertFailure "unexpected shape"
        _ -> assertFailure "unexpected shape"

  , testCase "conversion keeps the uniques of free variables" $ do
      let x = intId C.User "x" 5
          a = tyVar C.User "a" 7
          tm = C.TyApp (C.Var x) (C.VarTy a)
      withFreeVarsOf [tm] $ \env ->
        assertStructurallyEqual tm (toClashTerm (fromClashTerm env tm))

  ]
