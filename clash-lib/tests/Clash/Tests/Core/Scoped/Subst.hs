{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for substitution, going under binders, and moving values out of the
  scope of binders, in scoped Clash Core. Includes ports of the
  capture-avoidance tests in "Clash.Tests.Core.Subst".

  Most tests sink a term converted in a small scope into a scope with a free
  variable that has the same name as one of its binders, which therefore has
  to be renamed.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.Core.Scoped.Subst (tests) where

import qualified Control.Lens as Lens
import qualified Data.List as List
import Data.Maybe (isJust, isNothing)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Clash.Core.FreeVars as C
import Clash.Core.Literal (Literal (IntLiteral))
import qualified Clash.Core.Name as C
import qualified Clash.Core.Subst as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.FreeVars
import Clash.Core.Scoped.Internal.Scope (scopeWatermark)
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (mkLams, mkTyLams)

import Test.Clash.Rewrite
  ( assertAlphaEq, existentialDataCon, existentialTy, existentialTyVar, intId
  , intTy, localId, parseToTermQQ, tyVar)
import Test.Clash.Scoped
  ( LintScope, abstractFreeVars, aeqTermTyped, assertScopedAlphaEq, assertScopedTyAlphaEq
  , assertLintClean, assertTermAlphaEq, assertTermIs, atRaw, atScope, betaTm, betaTy
  , binderRaws, bindersNot, clashingBinders, closedTerm, extendFreeVars, forAllScrambled
  , lintScopeOf, lintTerm, reopenTerm, sinkInto, sunkD, termIn, testScope, varD
  , withClashing, withFreeVars, withTerm, withTestScope, withTests)
import Test.Clash.Scoped.Gen.Clash
  ( FreeVars (..), InScope (..), KnownCon (..), Redex (..), Sunk (..), ctxOf, genFreeVars
  , genInScope, genKnownCon, genRedex, genSunk, genType, inScopeOf, runG, shrinkInScope
  , shrinkKnownCon, shrinkRedex, shrinkSunk, showTerm, showType)

-- | Beta-reduce a redex with Clash's 'C.substTm', and with scoped Core
-- (converted to Clash Core), and check that the results agree. The function of
-- the redex is converted in the scope of its free variables, sunk into the
-- scope of those of the argument too, where its binders clash, and the
-- binders of both are scrambled.
betaAgrees :: Redex -> Property
betaAgrees redex = case redex of
  TmRedex fvs0 fvs1 x body arg ->
    let s = C.mkSubst (inScopeOf (fvs0 <> fvs1))
        expected = C.substTm "betaAgrees" (C.extendIdSubst s x arg) body
        d5 = C.varUniq x `elem` map C.varUniq (Lens.toListOf C.globalIds body)
    in  classify d5 "binder shares a unique with a global it mentions" $
        withFreeVars fvs0 $ \env0 -> do
          let fun = fromClashTerm env0 (C.Lam x body)
          extendFreeVars env0 fvs1 $ \env1 -> do
            let sc = convScope env1
            forAllScrambled sc (sinkInto sc fun) $ \fun1 ->
              forAllScrambled sc (fromClashTerm env1 arg) $ \arg1 ->
                resultAgrees expected (lintScopeOf env1 (fvs0 <> fvs1)) sc
                  (betaTm sc (App fun1 arg1))
  TyRedex fvs0 fvs1 a body ty ->
    let s = C.mkSubst (inScopeOf (fvs0 <> fvs1))
        expected = C.substTm "betaAgrees" (C.extendTvSubst s a ty) body
    in  withFreeVars fvs0 $ \env0 -> do
          let fun = fromClashTerm env0 (C.TyLam a body)
          extendFreeVars env0 fvs1 $ \env1 -> do
            let sc = convScope env1
            forAllScrambled sc (sinkInto sc fun) $ \fun1 ->
              resultAgrees expected (lintScopeOf env1 (fvs0 <> fvs1)) sc
                (betaTy sc (TyApp fun1 (fromClashType env1 ty)))

-- | Check the result of a substitution in scoped Core against that of Clash:
-- they must be alpha-equivalent, also in their types, and the result must be
-- well-scoped. Opening its binders must work as well.
resultAgrees :: Distinct n => C.Term -> LintScope -> Scope n -> Term n -> Property
resultAgrees expected ls sc res =
  let res1 = toClashTerm res
      (problems, _) = reopenTerm sc res
  in  counterexample ("Clash:\n" <> showTerm expected <> "\nscoped:\n" <> showTerm res1) $
        counterexample "aeqTerm" (C.aeqTerm expected res1) .&&.
        counterexample "aeqTermTyped" (aeqTermTyped expected res1) .&&.
        counterexample "lint" (lintTerm ls res === []) .&&.
        counterexample "reopen" (problems === [])

-- | Substitute for the variables that the pattern of an alternative binds, as
-- case-of-known-constructor does, with Clash's 'C.substTm' and in scoped Core
knownConAgrees :: KnownCon -> Property
knownConAgrees (KnownCon fvs0 fvs1 (pat, rhs) tys tms) = case pat of
  C.DataPat _ tvs ids ->
    let s0 = C.mkSubst (inScopeOf (fvs0 <> fvs1))
        expected = C.substTm "knownConAgrees"
          (C.extendIdSubstList (C.extendTvSubstList s0 (zip tvs tys)) (zip ids tms)) rhs
    in  withFreeVars fvs0 $ \env0 ->
          case fromClashTerm env0 (C.Case (C.Literal (IntLiteral 0)) intTy [(pat, rhs)]) of
            cs@Case {} -> extendFreeVars env0 fvs1 $ \env1 -> do
              let sc = convScope env1
              forAllScrambled sc (sinkInto sc cs) $ \case
                Case _ _ [Alt (DataPat _ tvs1 ids1) e] ->
                  case extendTySubstTele (idSubst sc) tvs1 (map (fromClashType env1) tys) >>= \s1 ->
                         extendTmSubstTele s1 ids1 (map (fromClashTerm env1) tms) of
                    Just s2 -> resultAgrees expected (lintScopeOf env1 (fvs0 <> fvs1)) sc (substTm s2 e)
                    Nothing -> counterexample "extendSubstTele" False
                _ -> counterexample "unexpected shape" False
            _ -> counterexample "unexpected shape" False
  _ -> discard

-- | The raw name of 'sunkD'
sunkRaw :: Int
sunkRaw = 1000000

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Subst"
  [ -- Substituting @[y := x]@ in @\x -> y@ must rename the binder @x@,
    -- rather than capture the free @x@.
    -- https://github.com/clash-lang/clash-compiler/pull/361
    testCase "substitution renames a lambda binder that would capture (#361)" $ do
      -- In the empty scope, y gets name 1 and z gets name 2
      let fun = closedTerm "\\(y :: Int) (z :: Int) -> y"
      -- x gets the name of z
      withTestScope [atRaw 2 (varD "x" "Int")] $ \ts -> do
        let res = betaTm (testScope ts) (App (sinkClosed fun) (termIn ts "x"))
        assertTermIs ts "\\(z :: Int) -> x" res
        assertLintClean ts res
        case res of
          Lam b _ -> assertBool "renamed" (tmNameRaw (tmNameOf b) /= 2)
          _ -> assertFailure "unexpected shape"

    -- Like the above, but the term variable is captured through its /type/:
    -- substituting @[y := (x :: a)]@ in @/\a -> y@ must rename the type binder.
    -- https://github.com/clash-lang/clash-compiler/pull/361
  , testCase "substitution renames a type lambda binder that would capture (#361)" $ do
      let y = intId C.User "y" 1
          b = tyVar C.User "b" 2
          -- In the empty scope, y gets name 1 and b gets name 2
          fun = fromClashTermClosed (C.Lam y (C.TyLam b (C.Var y)))
          a = tyVar C.User "a" 2
          x = localId C.User "x" 5 (C.VarTy a)
      withConvEnv [a] [x] $ \env -> do
        let res = betaTm (convScope env) (App (sinkClosed fun) (fromClashTerm env (C.Var x)))
        case res of
          TyLam tb (Var _ ty _) -> do
            assertBool "renamed" (tyNameRaw (tyNameOf tb) /= 2)
            assertScopedTyAlphaEq (C.VarTy a) ty
          _ -> assertFailure "unexpected shape"

    -- Substituting @[b := a]@ in @forall a. b@ must rename the binder @a@.
    -- https://github.com/clash-lang/clash-compiler/pull/361
  , testCase "substitution renames a forall binder that would capture (#361)" $ do
      let a = tyVar C.User "a" 2
          b = tyVar C.User "b" 1
          -- In the empty scope, b gets name 1 and a gets name 2
          ty = fromClashTypeClosed emptyScope (C.ForAllTy b (C.ForAllTy a (C.VarTy b)))
      withConvEnv [a] [] $ \env -> case sinkClosed ty of
        ForAllTy bb body -> do
          let sc = convScope env
              res = substTy (extendTySubst (idSubst sc) bb (fromClashType env (C.VarTy a))) body
          assertScopedTyAlphaEq (C.ForAllTy (tyVar C.User "c" 3) (C.VarTy a)) res
        _ -> assertFailure "unexpected shape"

    -- 'freshen' must rename a binder that clashes with the scope.
    -- https://github.com/clash-lang/clash-compiler/pull/361
  , testCase "freshen renames binders that clash (#361)" $
      withTerm "\\(x :: Int) (y :: Int) -> x" $ \ts0 tm0 ->
        withClashing ts0 [sunkD] tm0 $ \ts tm1 -> do
          let tm2 = freshen (testScope ts) tm1
          assertTermAlphaEq tm1 tm2
          assertBool "all binders renamed" (all (> sunkRaw) (binderRaws tm2))

    -- 'freshen' gives binders in different subterms the same name,
    -- 'freshenDistinct' doesn't
  , testCase "freshenDistinct gives sibling binders different names" $
      withTerm "(\\(x :: Int) -> x) ((\\(y :: Int) -> y) 1)" $ \ts0 tm0 ->
        withClashing ts0 [sunkD] tm0 $ \ts tm1 -> do
          let sc = testScope ts
              w = scopeWatermark sc
          binderRaws (freshen sc tm1) @?= [w + 1, w + 1]
          binderRaws (freshenDistinct sc tm1) @?= [w + 1, w + 2]

  , testCase "substitution keeps binders that don't clash" $ do
      let tm = [parseToTermQQ| (\(y :: Int) (z :: Int) -> y) (x_1 :: Int) |]
      withConvEnv [] [intId C.User "x" 1] $ \env ->
        case fromClashTerm env tm of
          app@(App (Lam _ (Lam bz _)) _) -> case betaTm (convScope env) app of
            Lam bz' _ -> tmNameRaw (tmNameOf bz') @?= tmNameRaw (tmNameOf bz)
            _ -> assertFailure "unexpected shape"
          _ -> assertFailure "unexpected shape"

    -- @(/\a -> \(x :: a) -> x) \@Int@: the type of the occurrence of @x@
    -- changes along with the type of its binder
  , testCase "substitution updates the types of occurrences" $ do
      let a = tyVar C.User "a" 1
          x = localId C.User "x" 2 (C.VarTy a)
          fun = fromClashTermClosed (C.TyLam a (C.Lam x (C.Var x)))
          res = betaTy emptyScope (TyApp fun (fromClashTypeClosed emptyScope intTy))
      case res of
        Lam b (Var _ ty _) -> do
          assertScopedTyAlphaEq intTy (idBinderType b)
          assertScopedTyAlphaEq intTy ty
        _ -> assertFailure "unexpected shape"

    -- An occurrence whose type mentions a variable that the type of its
    -- binder doesn't, as one could build by hand. Substitution must not leave
    -- a reference to the substituted variable.
  , testCase "substitution substitutes in the types of all occurrences" $ do
      let y = intId C.User "y" 1
      withConvEnv [] [y] $ \env -> case fromClashTerm env (C.Var y) of
        Var occ _ yn -> do
          let sc = convScope env
              occA = C.mkUnsafeSystemName "a" 0
              k = fromClashTypeClosed sc C.liftedTypeKind
              -- /\a -> (y :: a)
              tyLam = withFreshTyBinder sc occA k $ \_ ba ->
                TyLam ba (Var occ (tyBinderVar ba) (sink yn))
          case betaTy sc (TyApp tyLam (fromClashTypeClosed sc intTy)) of
            Var _ ty _ -> do
              assertBool "no free type variables" (nullNameSet (freeTyVarsOfType ty))
              assertScopedTyAlphaEq intTy ty
            _ -> assertFailure "unexpected shape"
        _ -> assertFailure "unexpected shape"

    -- @(/\a. case v of MkE \@b (y :: b) -> g \@a) \@b@, where the existential
    -- type variable of the pattern has the same name as the free type
    -- variable @b@
  , testCase "existential type variables don't capture type arguments" $ do
      let a = tyVar C.User "a" 70
          g = localId C.User "g" 71 (C.ForAllTy a (C.VarTy a))
          v = localId C.User "v" 72 existentialTy
          y = localId C.User "y" 73 (C.VarTy existentialTyVar)
          fun = C.TyLam a (C.Case (C.Var v) (C.VarTy a)
                  [(C.DataPat existentialDataCon [existentialTyVar] [y], C.TyApp (C.Var g) (C.VarTy a))])
          -- In the scope of g and v, a gets name 73 and the existential 74
          bFree = tyVar C.User "b" 74
          b' = tyVar C.User "b" 99
          y' = localId C.User "y" 98 (C.VarTy b')
      withConvEnv [] [g, v] $ \env0 -> do
        let fun0 = fromClashTerm env0 fun
        extendConvEnv env0 [bFree] [] $ \env1 -> do
          let res = betaTy (convScope env1)
                      (TyApp (sink fun0) (fromClashType env1 (C.VarTy bFree)))
          assertScopedAlphaEq
            (C.Case (C.Var v) (C.VarTy bFree)
              [(C.DataPat existentialDataCon [b'] [y'], C.TyApp (C.Var g) (C.VarTy bFree))])
            res

  , testCase "openAlt renames pattern binders that clash" $
      withTerm "\\(v :: E) -> (case v of { MkE (b :: Type) (y :: b) -> y }) :: E" $ \ts0 tm0 ->
        withClashing ts0 [sunkD] tm0 $ \ts tm -> case tm of
        Lam bv body -> do
          let sc = testScope ts
          -- The body of the lambda is in the scope of v, which has to be opened
          -- first
          openIdBinder sc bv body $ \sc1 bv1 body1 -> case body1 of
            Case subj1 ty1 [alt1] -> openAlt sc1 alt1 $ \_ p e -> do
              assertTermAlphaEq tm (Lam bv1 (Case subj1 ty1 [Alt p e]))
              case p of
                DataPat _ (TCons tb TNil) (TCons ib TNil) -> do
                  assertBool "existential renamed" (tyNameRaw (tyNameOf tb) > sunkRaw)
                  assertBool "field renamed" (tmNameRaw (tmNameOf ib) > sunkRaw)
                _ -> assertFailure "unexpected pattern"
            _ -> assertFailure "unexpected shape"
        _ -> assertFailure "unexpected shape"

  , testCase "unsinkIdBinder" $ do
      case fromClashTermClosed [parseToTermQQ| \(x :: Int) (y :: Int) -> x |] of
        Lam _ (Lam by e) -> assertBool "y doesn't occur" (isJust (unsinkIdBinder by e))
        _ -> assertFailure "unexpected shape"
      case fromClashTermClosed [parseToTermQQ| \(x :: Int) (y :: Int) -> y |] of
        Lam _ (Lam by e) -> assertBool "y occurs" (isNothing (unsinkIdBinder by e))
        _ -> assertFailure "unexpected shape"

  , testCase "teleIdBinderTypes" $ do
      -- @letrec (x :: Int) = 1; (y :: a) = y in x@, with a free type variable
      -- @a@
      let a = tyVar C.User "a" 1
          x = intId C.User "x" 2
          y = localId C.User "y" 3 (C.VarTy a)
          tm = C.Letrec [(x, C.Literal (IntLiteral 1)), (y, C.Var y)] (C.Var x)
      withConvEnv [a] [] $ \env -> case fromClashTerm env tm of
        Let (Rec bs _) _ -> case teleIdBinderTypes bs of
          [t1, t2] -> do
            assertScopedTyAlphaEq intTy t1
            assertScopedTyAlphaEq (C.VarTy a) t2
          tys -> assertFailure ("unexpected number of types: " <> show (length tys))
        _ -> assertFailure "unexpected shape"

    -- The existential type variable only occurs in the type of an occurrence
    -- of a variable from outside the alternative, as one could build by hand
  , testCase "unsinkAlt looks at the types of occurrences" $ do
      -- @case v of MkE \@b (y :: b) -> y@
      let v = localId C.User "v" 1 existentialTy
          y = localId C.User "y" 2 (C.VarTy existentialTyVar)
          w = intId C.User "w" 3
          tm = C.Case (C.Var v) existentialTy [(C.DataPat existentialDataCon [existentialTyVar] [y], C.Var y)]
      withConvEnv [] [v, w] $ \env -> case (fromClashTerm env (C.Var w), fromClashTerm env tm) of
        (wVar@(Var occW _ wn), Case _ _ [alt]) -> openAlt (convScope env) alt $ \sc1 p e -> case e of
          Var _ exTy _ -> do
            assertBool "w occurs at the existential type"
              (isNothing (unsinkAlt p (Var occW exTy (sink wn))))
            assertBool "w occurs at its own type"
              (isJust (unsinkAlt p (sink wVar)))
            assertBool "the existential occurs"
              (isNothing (strengthenTyPat p exTy))
            assertBool "Int doesn't mention the existential"
              (isJust (strengthenTyPat p (fromClashTypeClosed sc1 intTy)))
          _ -> assertFailure "unexpected shape"
        _ -> assertFailure "unexpected shape"

    -- Code that builds terms, e.g. primitive reductions, creates fresh binders
    -- in a pure context
  , testCase "closed terms can be built with fresh binders" $ do
      let occ = C.mkUnsafeSystemName "x" 0
          tm = withFreshIdBinder emptyScope occ (fromClashTypeClosed emptyScope intTy) $
                 \_ b -> toClashTerm (Lam b (idBinderVar b))
      assertAlphaEq [parseToTermQQ| \(x :: Int) -> x |] tm

  , testCase "going under a sunk binder that clashes renames it" $
      -- In the empty scope, the binder of the lambda gets name 1
      withTerm "\\(y :: Int) -> y" $ \ts0 lam0 ->
        -- So does x, which the binder now shadows
        withClashing ts0 [varD "x" "Int"] lam0 $ \ts lam1 -> do
          clashingBinders ts lam1 @?= [1]
          assertTermIs ts "\\(y :: Int) -> y" lam1
          -- Going under it renames it
          case lam1 of
            Lam b1 e1 -> openIdBinder (testScope ts) b1 e1 $ \_ b2 _ ->
              assertBool "renamed" (tmNameRaw (tmNameOf b2) /= 1)
            _ -> assertFailure "unexpected shape"
  , testCase "extendTmSubstTele needs as many terms as binders" $ do
      let tm = [parseToTermQQ| let { (x :: Int) = x; (y :: Int) = x } in y |]
      case fromClashTermClosed tm of
        Let (Rec bs _) _ -> do
          let s = idSubst emptyScope
              lit = Literal (IntLiteral 1)
          assertBool "too few" (isNothing (extendTmSubstTele s bs [lit]))
          assertBool "exactly" (isJust (extendTmSubstTele s bs [lit, lit]))
          assertBool "too many" (isNothing (extendTmSubstTele s bs [lit, lit, lit]))
        _ -> assertFailure "unexpected shape"

  , testCase "withFreshIdBinders" $ do
      let x = intId C.User "x" 7
      withConvEnv [] [x] $ \env -> do
        let sc = convScope env
            int = fromClashTypeClosed sc intTy
            occ = C.mkUnsafeSystemName "f" 0
        withFreshIdBinders sc [(occ, int), (occ, int)] $ \_ bs vars -> case (bs, vars) of
          (TCons b1 (TCons b2 TNil), [Var _ _ n1, Var _ _ n2]) -> do
            [tmNameRaw (tmNameOf b1), tmNameRaw (tmNameOf b2)] @?= [8, 9]
            [tmNameRaw n1, tmNameRaw n2] @?= [8, 9]
          _ -> assertFailure "unexpected shape"

  , withTests 500 $ testGroup "properties"
      [ testProperty "substitution agrees with Clash" $
          forAllShrink genRedex shrinkRedex betaAgrees

      , testProperty "substitution for variables of the scope agrees with Clash" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) -> case fvIds fvs0 of
            [] -> property True
            x : _ ->
              let lit = C.Literal (IntLiteral 1)
                  s0 = C.mkSubst (inScopeOf (fvs0 <> fvs1))
                  expected = C.substTm "tmVarsSubst" (C.extendIdSubst s0 x lit) tm
              in  withFreeVars fvs0 $ \env0 -> do
                    let tm0 = fromClashTerm env0 tm
                    extendFreeVars env0 fvs1 $ \env1 -> do
                      let sc = convScope env1
                      forAllScrambled sc (sinkInto sc tm0) $ \tm1 -> case fromClashTerm env1 (C.Var x) of
                        Var _ _ xn ->
                          resultAgrees expected (lintScopeOf env1 (fvs0 <> fvs1)) sc
                            (substTm (tmVarsSubst sc (singletonNameMap xn (Literal (IntLiteral 1)))) tm1)
                        _ -> counterexample "not a variable" False

      , testProperty "substitution for the variables of a pattern agrees with Clash" $
          forAllShrink genKnownCon shrinkKnownCon knownConAgrees

        -- A binder in a value may have the name of a variable in scope, of
        -- either sort, if that doesn't occur in its scope: e.g. that of a
        -- binder created for the value. Substitution must not substitute for
        -- it.
      , testProperty "substitution keeps binders that shadow the substituted one" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
            withFreeVars fvs $ \env -> do
              let sc = convScope env
                  body = fromClashTerm env tm
                  int = fromClashTypeClosed sc intTy
                  kind = fromClashTypeClosed sc C.liftedTypeKind
                  -- The binders get the name just above the watermark of the
                  -- scope, like the first binder of the body
                  lam = withFreshIdBinder sc (C.mkUnsafeSystemName "x" 0) int $ \sc1 b ->
                          Lam b (sinkInto sc1 body)
                  tyLam = withFreshTyBinder sc (C.mkUnsafeSystemName "a" 0) kind $ \sc1 b ->
                            TyLam b (sinkInto sc1 body)
                  ls = lintScopeOf env fvs
              resultAgrees tm ls sc (betaTm sc (App lam (Literal (IntLiteral 1)))) .&&.
                resultAgrees tm ls sc (betaTy sc (TyApp tyLam int))

        -- Every binder gets the name just above the watermark of the scope at
        -- it: binders in different subterms may get the same name.
      , testProperty "freshen is alpha-equivalent and renames binders just above the watermark" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) ->
            withFreeVars fvs0 $ \env0 -> do
              let tm0 = fromClashTerm env0 tm
              extendFreeVars env0 fvs1 $ \env1 -> do
                let sc = convScope env1
                forAllScrambled sc (sinkInto sc tm0) $ \tm1 -> do
                  let tm2 = freshen sc tm1
                  counterexample ("freshened:\n" <> showTerm (toClashTerm tm2)) $
                    counterexample "aeqTm" (aeqTm tm1 tm2) .&&.
                    counterexample "aeqTerm" (C.aeqTerm tm (toClashTerm tm2)) .&&.
                    counterexample "binders just above the watermark"
                      (bindersNot (\w k -> k == w + 1) (scopeWatermark sc) tm2 === []) .&&.
                    lintTerm (lintScopeOf env1 (fvs0 <> fvs1)) tm2 === []

        -- Every binder gets a different name, numbered like conversion does
      , testProperty "freshenDistinct is alpha-equivalent and numbers binders like conversion" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) ->
            withFreeVars fvs0 $ \env0 -> do
              let tm0 = fromClashTerm env0 tm
              extendFreeVars env0 fvs1 $ \env1 -> do
                let sc = convScope env1
                    w = scopeWatermark sc
                forAllScrambled sc (sinkInto sc tm0) $ \tm1 -> do
                  let tm2 = freshenDistinct sc tm1
                  counterexample ("freshened:\n" <> showTerm (toClashTerm tm2)) $
                    counterexample "aeqTm" (aeqTm tm1 tm2) .&&.
                    counterexample "aeqTerm" (C.aeqTerm tm (toClashTerm tm2)) .&&.
                    counterexample "binders numbered from the watermark"
                      (binderRaws tm2 === [w + 1 .. w + length (binderRaws tm1)]) .&&.
                    lintTerm (lintScopeOf env1 (fvs0 <> fvs1)) tm2 === []

        -- Conversion numbers binders like 'freshenDistinct' does
      , testProperty "freshenDistinct keeps a converted term as it is" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
            withFreeVars fvs $ \env -> do
              let tm1 = fromClashTerm env tm
              show (freshenDistinct (convScope env) tm1) === show tm1

        -- See "hasFreeVars agrees with free variables"
      , testProperty "closeTerm succeeds exactly when a term has no free variables" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) -> forAll arbitrary $ \abstract ->
            withFreeVars fvs0 $ \env0 -> do
              let tm0
                    | abstract = sinkClosed (fromClashTermClosed (abstractFreeVars fvs0 tm))
                    | otherwise = fromClashTerm env0 tm
              extendFreeVars env0 fvs1 $ \env1 -> do
                let sc = convScope env1
                forAllScrambled sc (sinkInto sc tm0) $ \tm1 -> do
                  let closed = nullNameSet (freeTmVars tm1) && nullNameSet (freeTyVars tm1)
                  cover 20 closed "no free variables" $ case closeTerm tm1 of
                    Nothing -> counterexample "closeTerm failed" (not closed)
                    Just tm2 -> counterexample "closeTerm succeeded" closed .&&.
                      show (atScope sc (sinkClosed tm2)) === show tm1 .&&.
                      lintTerm (lintScopeOf emptyConvEnv (FreeVars [] [])) tm2 === []

      , testProperty "closeType succeeds exactly when a type has no free variables" $
          forAllBlind genFreeVars $ \fvs ->
            forAllShow (runG 30 (genType (ctxOf fvs) C.liftedTypeKind 3)) showType $ \ty ->
              forAll arbitrary $ \abstract ->
                withFreeVars fvs $ \env -> do
                  let ty1
                        | abstract = fromClashTypeClosed (convScope env) (foldr C.ForAllTy ty (fvTyVars fvs))
                        | otherwise = fromClashType env ty
                      closed = nullNameSet (freeTyVarsOfType ty1)
                  cover 20 closed "no free variables" $ case closeType ty1 of
                    Nothing -> counterexample "closeType failed" (not closed)
                    Just ty2 -> counterexample "closeType succeeded" closed .&&.
                      show (atScope (convScope env) (sinkClosed ty2)) === show ty1

      , testProperty "opening binders is alpha-equivalent and renames binders that clash" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) ->
            withFreeVars fvs0 $ \env0 -> do
              let tm0 = fromClashTerm env0 tm
              extendFreeVars env0 fvs1 $ \env1 -> do
                let sc = convScope env1
                forAllScrambled sc (sinkInto sc tm0) $ \tm1 -> do
                  let (problems, tm2) = reopenTerm sc tm1
                  counterexample ("reopened:\n" <> showTerm (toClashTerm tm2)) $
                    problems === [] .&&.
                    counterexample "aeqTm" (aeqTm tm1 tm2) .&&.
                    counterexample "aeqTerm" (C.aeqTerm tm (toClashTerm tm2)) .&&.
                    lintTerm (lintScopeOf env1 (fvs0 <> fvs1)) tm2 === []

        -- Conversion numbers binders such that they never clash
      , testProperty "opening the binders of a converted term keeps them" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
            withFreeVars fvs $ \env -> do
              let tm1 = fromClashTerm env tm
                  (problems, tm2) = reopenTerm (convScope env) tm1
              problems === [] .&&. binderRaws tm2 === binderRaws tm1 .&&.
                property (aeqTm tm1 tm2)
      ]
  , testGroup "closing over variables" closeOverTests
  , withTests 500 $ testGroup "closing over variables, properties"
      [ testProperty "freeVarOccs visits the free variables in the order Clash does" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) ->
            withFreeVars fvs0 $ \env0 -> do
              let tm0 = fromClashTerm env0 tm
              extendFreeVars env0 fvs1 $ \env1 -> do
                let sc = convScope env1
                forAllScrambled sc (sinkInto sc tm0) $ \tm1 ->
                  let (tys, tms) = freeVarOccs tm1
                      (cTys, cTms) = clashFreeVarOrder tm
                  in  map tyOccRaw tys === cTys .&&. map tmOccRaw tms === cTms
      , testProperty "abstracting over the free variables agrees with Clash" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
            withFreeVars fvs $ \env -> do
              let tm1 = fromClashTerm env tm
                  (tys, tms) = freeVarOccs tm1
                  tvsC = C.dependencySortTyVars (List.nubBy sameUniq (Lens.toListOf C.termFreeTyVars tm))
                  idsC = List.nubBy sameUniq (Lens.toListOf C.freeLocalIds tm)
                  -- The scoped type variables in the order of the Clash ones
                  tys1 = [ty | tv <- tvsC, ty <- tys, tyOccRaw ty == fromIntegral (C.varUniq tv)]
                  expected = C.mkTyLams (C.mkLams tm idsC) tvsC
              case closeOver tys1 tms (\_ tvTele idTele cl -> (\t -> mkTyLams (mkLams t idTele) tvTele) <$> closureTerm cl tm1) of
                Just (Just closed) ->
                  let res = toClashTerm closed in
                  counterexample ("Clash:\n" <> showTerm expected <> "\nscoped:\n" <> showTerm res) $
                    property (aeqTermTyped expected res)
                _ -> counterexample "closeOver failed" False
      ]
  , testCase "teleIdVars gives the variables of a telescope in its scope" $ do
      let tm = [parseToTermQQ| let { (x :: Int) = y; (y :: Int) = x } in x |]
      case fromClashTermClosed tm of
        Let bind@(Rec _ _) body -> openBind emptyScope bind body $ \_ bind1 body1 -> case bind1 of
          Rec bs xs -> case teleIdVars bs of
            -- The variables are those the right-hand sides and the body refer
            -- to
            [xVar, yVar] -> do
              assertBool "x" (aeqTm xVar body1)
              assertBool "x and y" (and (zipWith aeqTm [yVar, xVar] xs))
            _ -> assertFailure "unexpected variables"
          NonRec {} -> assertFailure "unexpected shape"
        _ -> assertFailure "unexpected shape"

  , testCase "patVars gives the variables of a pattern in its scope" $ do
      -- @case v of MkE \@b (y :: b) -> y@
      let v = localId C.User "v" 1 existentialTy
          y = localId C.User "y" 2 (C.VarTy existentialTyVar)
          tm = C.Case (C.Var v) existentialTy [(C.DataPat existentialDataCon [existentialTyVar] [y], C.Var y)]
      withConvEnv [] [v] $ \env -> case fromClashTerm env tm of
        Case _ _ [alt] -> openAlt (convScope env) alt $ \_ p e -> case patVars p of
          ([VarTy _ _ tn], [yVar@(Var _ yTy _)]) -> do
            assertBool "the variable of the field" (aeqTm yVar e)
            -- The type of the field is the existential type variable
            case yTy of
              VarTy _ _ tn' -> tyNameRaw tn' @?= tyNameRaw tn
              _ -> assertFailure "unexpected type"
          _ -> assertFailure "unexpected variables"
        _ -> assertFailure "unexpected shape"
  ]

-- * Closing over variables

-- | The raw name of a type variable occurrence
tyOccRaw :: Type n -> Int
tyOccRaw = \case
  VarTy _ _ n -> tyNameRaw n
  _ -> error "tyOccRaw: not a type variable"

-- | The raw name of a term variable occurrence
tmOccRaw :: Term n -> Int
tmOccRaw = \case
  Var _ _ n -> tmNameRaw n
  _ -> error "tmOccRaw: not a variable"

sameUniq :: C.Var a -> C.Var a -> Bool
sameUniq v1 v2 = C.varUniq v1 == C.varUniq v2

-- | The uniques of the free type variables and the free term variables of a
-- Clash term, in the order 'C.freeLocalVars' visits them first
clashFreeVarOrder :: C.Term -> ([Int], [Int])
clashFreeVarOrder tm =
  ( List.nub [fromIntegral (C.varUniq v) | v <- vs, isTyVar v]
  , List.nub [fromIntegral (C.varUniq v) | v <- vs, not (isTyVar v)] )
 where
  isTyVar = \case
    C.TyVar {} -> True
    C.Id {} -> False

  vs :: [C.Var ()]
  vs = Lens.toListOf C.freeLocalVars tm

closeOverTests :: [TestTree]
closeOverTests =
  [ testCase "binds the variables with their names, kinds and types" $
      withConvEnv [k, a] [x] $ \env -> do
        let res = closeOver [tyOcc env k, tyOcc env a] [tmOcc env x] $ \_ tvTele idTele _ ->
              case (tvTele, idTele) of
                (TCons bk (TCons ba TNil), TCons bx TNil) ->
                  [ tyNameRaw (tyNameOf bk), tyNameRaw (tyNameOf ba), tmNameRaw (tmNameOf bx) ]
                    == [51, 50, 31]
                    && C.aeqType (toClashType (tyBinderKind ba)) (C.VarTy k)
                    && C.aeqType (toClashType (idBinderType bx)) (C.VarTy a)
                _ -> False
        res @?= Just True
  , testCase "closes terms and types that only mention the variables" $
      withConvEnv [k, a] [x, y] $ \env -> do
        let res = closeOver [tyOcc env k, tyOcc env a] [tmOcc env x] $ \_ _ _ cl ->
              ( isJust (closureTerm cl (fromClashTerm env (C.Var x)))
              , isNothing (closureTerm cl (fromClashTerm env (C.Var y)))
              , isJust (closureType cl (fromClashType env (C.VarTy a)))
              )
        res @?= Just (True, True, True)
  , testCase "rejects a term whose types mention other type variables" $
      withConvEnv [k, a] [x] $ \env -> do
        let res = closeOver [tyOcc env k] [] $ \_ _ _ cl ->
              isNothing (closureTerm cl (fromClashTerm env (C.Var x)))
        res @?= Just True
  , testCase "rejects a type variable before the type variables in its kind" $
      withConvEnv [k, a] [] $ \env ->
        assertBool "Nothing" (isNothing (closeOver [tyOcc env a, tyOcc env k] [] (\_ _ _ _ -> ())))
  , testCase "rejects a term variable whose type mentions other type variables" $
      withConvEnv [k, a] [x] $ \env ->
        assertBool "Nothing" (isNothing (closeOver [tyOcc env k] [tmOcc env x] (\_ _ _ _ -> ())))
  , testCase "rejects a variable given twice" $
      withConvEnv [] [y] $ \env ->
        assertBool "Nothing" (isNothing (closeOver [] [tmOcc env y, tmOcc env y] (\_ _ _ _ -> ())))
  , testCase "freeVarOccs gives the occurrences of free variables, not bound ones" $
      withConvEnv [k, a] [x, y] $ \env -> do
        -- @\(z :: a) -> x y z@
        let z = localId C.User "z" 40 (C.VarTy a)
            tm = fromClashTerm env (C.Lam z (C.mkTmApps (C.Var x) [C.Var y, C.Var z]))
            (tys, tms) = freeVarOccs tm
        (map tyOccRaw tys, map tmOccRaw tms) @?= ([50, 51], [31, 32])
  ]
 where
  k = tyVar C.User "k" 51
  a = C.TyVar (C.mkUnsafeSystemName "a" 50) 50 (C.VarTy k)
  x = localId C.User "x" 31 (C.VarTy a)
  y = intId C.User "y" 32

  tyOcc :: Distinct n => ConvEnv n -> C.TyVar -> Type n
  tyOcc env tv = fromClashType env (C.VarTy tv)

  tmOcc :: Distinct n => ConvEnv n -> C.Id -> Term n
  tmOcc env i = fromClashTerm env (C.Var i)
