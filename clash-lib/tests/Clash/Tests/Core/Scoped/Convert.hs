{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the conversion between Clash Core and scoped Clash Core.

  The properties of scoped Clash Core run on generated terms
  ("Test.Clash.Scoped.Gen"), with the number of tests that 'withTests' gives
  them. To run more tests, on larger terms, e.g. after changing the trusted
  core:

  > cabal run clash-lib:unittests -- -p Scoped --quickcheck-tests 100000 --quickcheck-max-size 200
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.Core.Scoped.Convert (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Clash.Core.Name as C
import qualified Clash.Core.Subst as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Internal.Scope (scopeWatermark)
import Clash.Core.Scoped.Pretty ()
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite
  ( assertAlphaEq, assertErrorContains, assertNoShadowing, assertStructurallyEqual
  , existentialDataCon, existentialTy, existentialTyVar, globalId, intId, intTy
  , localId, parseToTermQQ, shadowingBinders, showPprU, tyVar)
import Test.Clash.Scoped
  ( aeqTermTyped, binderRaws, extendFreeVars, forAllScrambled, lintScopeOf, lintTerm
  , lowBinders, roundTrip, sinkInto, withFreeVars, withFreeVarsOf, withTests)
import Test.Clash.Scoped.Gen
  ( InScope (..), Sunk (..), ctxOf, freeVarsUniqs, genFreeVars, genInScope, genSunk
  , genType, kindOf, runG, shrinkInScope, shrinkSunk, showFreeVars, showTerm
  , showType, termFeatures, wellScopedIn)

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

  , testCase "pretty printing shows the converted term" $ do
      let tm = fromClashTermClosed [parseToTermQQ| \(x :: Int) (y :: Int) -> x |]
      showPprU tm @?= showPprU (toClashTerm tm)

  , testCase "conversion keeps the uniques of free variables" $ do
      let x = intId C.User "x" 5
          a = tyVar C.User "a" 7
          tm = C.TyApp (C.Var x) (C.VarTy a)
      withFreeVarsOf [tm] $ \env ->
        assertStructurallyEqual tm (toClashTerm (fromClashTerm env tm))


  , withTests 500 $ testGroup "properties"
      [ testProperty "generated terms are well-scoped" $
          forAll genInScope $ \(InScope fvs tm) -> do
            let fs = termFeatures fvs tm
                coverF p f = cover p (f `elem` fs) f
            coverF 30 "shadowing binder" $
              coverF 20 "binder shares a unique with a variable of the other sort" $
              coverF 30 "binder shares a unique with a global" $
              coverF 30 "free variable shares a unique with a global" $
              coverF 30 "field type mentions an existential" $
              tabulate "features" fs (wellScopedIn fvs tm)

        -- Kinds are plausible, but some types of kind variables can't be
        -- generated, so this only checks types of kind Type
      , testProperty "generated types have the kind they are generated at" $
          forAllBlind genFreeVars $ \fvs ->
            forAllShow (runG 30 (genType (ctxOf fvs) C.liftedTypeKind 3)) showType $ \ty ->
              counterexample (showFreeVars fvs) $
                fmap showType (kindOf ty) === Just (showType C.liftedTypeKind)

      , testProperty "round trip is alpha-equivalent" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
            withFreeVars fvs $ \env -> do
              let tm1 = toClashTerm (fromClashTerm env tm)
              counterexample ("round trip:\n" <> showTerm tm1) $
                C.aeqTerm tm tm1 .&&. aeqTermTyped tm tm1

        -- The free variables keep their uniques, so the round trip of a term
        -- that conversion has numbered is the same term
      , testProperty "conversion is idempotent" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
            withFreeVars fvs $ \env -> do
              let tm1 = toClashTerm (fromClashTerm env tm)
              property (C.eqTerm (toClashTerm (fromClashTerm env tm1)) tm1)

      , testProperty "conversion numbers binders depth-first above the watermark" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
            withFreeVars fvs $ \env -> do
              let raws = binderRaws (fromClashTerm env tm)
                  wm = scopeWatermark (convScope env)
              raws === take (length raws) [wm + 1 ..]

      , testProperty "converted terms are well-scoped" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
            withFreeVars fvs $ \env ->
              lintTerm (lintScopeOf env fvs) (fromClashTerm env tm) === []

        -- The free variables keep their uniques, and the watermark is the
        -- largest of them
      , testProperty "the watermark of a conversion environment bounds its names" $
          forAll genSunk $ \(Sunk fvs0 fvs1 _) ->
            withFreeVars fvs0 $ \env0 -> extendFreeVars env0 fvs1 $ \env1 -> do
              let uniqs = map fromIntegral (freeVarsUniqs (fvs0 <> fvs1))
              scopeWatermark (convScope env1) === maximum (0 : uniqs)

        -- Sinking a converted term into a scope with more variables leaves
        -- binders that clash with them, but means the same
      , testProperty "sinking is alpha-equivalent to converting in the larger scope" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) ->
            withFreeVars fvs0 $ \env0 -> do
              let tm0 = fromClashTerm env0 tm
              extendFreeVars env0 fvs1 $ \env1 -> do
                let tm1 = sinkInto (convScope env1) tm0
                property (aeqTm tm1 (fromClashTerm env1 tm)) .&&.
                  lintTerm (lintScopeOf env1 (fvs0 <> fvs1)) tm1 === []

        -- Binders that shadow variables in scope, and each other, convert to
        -- Clash binders that shadow them too
      , testProperty "scrambled binders convert to Clash" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) ->
            withFreeVars fvs0 $ \env0 -> do
              let tm0 = fromClashTerm env0 tm
              extendFreeVars env0 fvs1 $ \env1 -> do
                let sc = convScope env1
                forAllScrambled sc (sinkInto sc tm0) $ \tm1 -> do
                  let tm2 = toClashTerm tm1
                  classify (not (null (lowBinders (scopeWatermark sc) tm1)))
                    "binders at or below the watermark" $
                    classify (not (null (shadowingBinders tm2))) "term binders that shadow" $
                    counterexample "aeqTm" (aeqTm tm1 (fromClashTerm env1 tm)) .&&.
                    counterexample "aeqTerm" (C.aeqTerm tm tm2) .&&.
                    counterexample "aeqTermTyped" (aeqTermTyped tm tm2) .&&.
                    lintTerm (lintScopeOf env1 (fvs0 <> fvs1)) tm1 === []
      ]
  ]
