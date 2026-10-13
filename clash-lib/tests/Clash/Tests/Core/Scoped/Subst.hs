{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for substitution, going under binders, and moving values out of the
  scope of binders, in scoped Clash Core.

  Most tests sink a term built in a small scope into a scope with a free
  variable that has the same name as one of its binders, which therefore has
  to be renamed. The properties compare substitution with 'naiveSubst', a
  substitution that doesn't rename, on terms whose binders can't capture.
-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clash.Tests.Core.Scoped.Subst (tests) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import Data.Maybe (isJust, isNothing)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Unsafe.Coerce (unsafeCoerce)

import qualified Clash.Core.Name as C
import Clash.Core.Literal (Literal (IntLiteral))

import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.FreeVars
import Clash.Core.Scoped.Internal.Scope (scopeWatermark)
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (mkLams, mkTyLams)
import qualified Clash.Core.Scoped.TysPrim as P

import Test.Clash.Scoped
  ( TestScope (..), TestType, assertLintClean, assertTermAlphaEq, assertTermIs, assertTypeIs, atRaw
  , atScope, betaTm, betaTy, binderRaws, bindersNot, clashingBinders, closedTerm, closedType
  , emptyTestScope, extendTestScope, forAllScrambled, kindedTyVarD, lintTerm
  , reopenTerm, sinkInto, sunkD, termIn, testLintScope, tyVarD, typeIn, varD, withClashing, withTerm
  , withTestScope, withTests)
import Test.Clash.Scoped.Gen
  ( InScope (..), KnownCon (..), Redex (..), SomeScope (..), Sunk (..), ctxOf, genClosedSunk
  , genInScope, genKnownCon, genRedex, genSunk, genTestScope, genType, refFreeTmVars
  , refFreeTyVars, shrinkInScope, shrinkKnownCon, shrinkRedex, shrinkSunk, showT, showTy)

-- * Substitution without renaming

-- | Substitute types for type variables and terms for term variables, by raw
-- name, without renaming binders. The values are in the scope of the term.
-- This is substitution if no binder in the term binds a variable that is
-- substituted, nor a free variable of the values: e.g. if the binders of the
-- term are 'freshenDistinct'ed in a scope with the free variables of the
-- values. The oracle for the substitution of "Clash.Core.Scoped.Subst".
naiveSubst :: IntMap.IntMap (Type n) -> IntMap.IntMap (Term n) -> Term n -> Term n
naiveSubst tys tms = goTm
 where
  -- Trusted: the values are put under binders without sinking them, which is
  -- fine under the precondition: no binder captures their free variables
  coerceTy :: Type n -> Type x
  coerceTy = unsafeCoerce
  coerceTm :: Term n -> Term x
  coerceTm = unsafeCoerce

  goTy :: Type x -> Type x
  goTy = \case
    VarTy occ k nm -> maybe (VarTy occ (goTy k) nm) coerceTy (IntMap.lookup (tyNameRaw nm) tys)
    ForAllTy b t -> ForAllTy (goTyB b) (goTy t)
    AppTy t1 t2 -> AppTy (goTy t1) (goTy t2)
    AnnType a t -> AnnType a (goTy t)
    t -> t

  goTyB :: TyBinder x y -> TyBinder x y
  goTyB b = b {tyBinderKind = goTy (tyBinderKind b)}

  goIdB :: IdBinder x y -> IdBinder x y
  goIdB b = b {idBinderType = goTy (idBinderType b)}

  goTele :: (forall a c. b a c -> b a c) -> Tele b x y -> Tele b x y
  goTele _ TNil = TNil
  goTele f (TCons b bs) = TCons (f b) (goTele f bs)

  goTm :: Term x -> Term x
  goTm = \case
    Var occ ty nm -> maybe (Var occ (goTy ty) nm) coerceTm (IntMap.lookup (tmNameRaw nm) tms)
    Lam b e -> Lam (goIdB b) (goTm e)
    TyLam b e -> TyLam (goTyB b) (goTm e)
    App e1 e2 -> App (goTm e1) (goTm e2)
    TyApp e ty -> TyApp (goTm e) (goTy ty)
    Let (NonRec b x) e -> Let (NonRec (goIdB b) (goTm x)) (goTm e)
    Let (Rec bs xs) e -> Let (Rec (goTele goIdB bs) (map goTm xs)) (goTm e)
    Case e ty alts -> Case (goTm e) (goTy ty) (map goAlt alts)
    Cast e t1 t2 -> Cast (goTm e) (goTy t1) (goTy t2)
    Tick t e -> Tick (goTick t) (goTm e)
    e -> e

  goAlt :: Alt x -> Alt x
  goAlt (Alt p e) = case p of
    DataPat dc tvs ids -> Alt (DataPat dc (goTele goTyB tvs) (goTele goIdB ids)) (goTm e)
    LitPat _ -> Alt p (goTm e)
    DefaultPat -> Alt p (goTm e)

  goTick :: TickInfo x -> TickInfo x
  goTick = \case
    NameMod m ty -> NameMod m (goTy ty)
    Attributes ty e -> Attributes (goTy ty) (goTm e)
    t -> t

-- | Check the result of a substitution: it must be alpha-equivalent to the
-- expected one, and well-scoped, and opening its binders must work as well
resultAgrees :: Distinct n => TestScope n -> Term n -> Term n -> Property
resultAgrees ts expected res =
  let (problems, _) = reopenTerm (testScope ts) res
  in  counterexample ("expected:\n" <> showT expected <> "\nbut got:\n" <> showT res) $
        counterexample "aeqTm" (aeqTm expected res) .&&.
        counterexample "lint" (lintTerm (testLintScope ts) res === []) .&&.
        counterexample "reopen" (problems === [])

-- | Beta-reduce a redex, and compare the result with that of 'naiveSubst'.
-- The function of the redex is built in a scope, sunk into the scope of the
-- argument too, where its binders clash, and the binders of both are
-- scrambled.
betaAgrees :: Redex -> Property
betaAgrees = \case
  TmRedex _ ts1 fun0 arg -> do
    let sc = testScope ts1
    case freshenDistinct sc (sink fun0) of
      Lam b body -> do
        let expected = naiveSubst IntMap.empty (IntMap.singleton (tmNameRaw (tmNameOf b)) arg) (unsafeUnsink body)
        forAllScrambled sc (sink fun0) $ \fun1 ->
          forAllScrambled sc arg $ \arg1 ->
            resultAgrees ts1 expected (betaTm sc (App fun1 arg1))
      _ -> property False
  TyRedex _ ts1 fun0 ty -> do
    let sc = testScope ts1
    case freshenDistinct sc (sink fun0) of
      TyLam b body -> do
        let expected = naiveSubst (IntMap.singleton (tyNameRaw (tyNameOf b)) ty) IntMap.empty (unsafeUnsink body)
        forAllScrambled sc (sink fun0) $ \fun1 ->
          resultAgrees ts1 expected (betaTy sc (TyApp fun1 ty))
      _ -> property False

-- | Move the body of a binder out of its scope, for 'naiveSubst', which
-- substitutes for the variable it binds
unsafeUnsink :: Term l -> Term n
unsafeUnsink = unsafeCoerce

-- | Substitute for the variables that the pattern of an alternative binds, as
-- case-of-known-constructor does, and compare the result with that of
-- 'naiveSubst'
knownConAgrees :: KnownCon -> Property
knownConAgrees (KnownCon _ ts1 alt tys tms) = do
  let sc = testScope ts1
      cs = Case (Literal (IntLiteral 0)) (sinkClosed (closedType "Int")) [sink alt]
  case freshenDistinct sc cs of
    Case _ _ [Alt (DataPat _ tvs0 ids0) e0] -> do
      let expected = naiveSubst
            (IntMap.fromList (zip (teleRaws tyNameRaw tyNameOf tvs0) tys))
            (IntMap.fromList (zip (teleRaws tmNameRaw tmNameOf ids0) tms))
            (unsafeUnsink e0)
      forAllScrambled sc cs $ \case
        Case _ _ [Alt (DataPat _ tvs1 ids1) e] ->
          case extendTySubstTele (idSubst sc) tvs1 tys >>= \s1 -> extendTmSubstTele s1 ids1 tms of
            Just s2 -> resultAgrees ts1 expected (substTm s2 e)
            Nothing -> counterexample "extendSubstTele" False
        _ -> counterexample "unexpected shape" False
    _ -> discard

-- | The raw names a telescope binds
teleRaws :: (forall z. k z -> Int) -> (forall x' y'. b x' y' -> k y') -> Tele b x y -> [Int]
teleRaws _ _ TNil = []
teleRaws raw nm (TCons b bs) = raw (nm b) : teleRaws raw nm bs

-- | The raw name of 'sunkD'
sunkRaw :: Int
sunkRaw = 1000000

intTy :: Distinct n => Type n
intTy = sinkClosed (closedType "Int")

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
    -- substituting @[y := (x :: a)]@ in @/\b -> y@ must rename the type binder.
    -- https://github.com/clash-lang/clash-compiler/pull/361
  , testCase "substitution renames a type lambda binder that would capture (#361)" $ do
      -- In the empty scope, y gets name 1 and b gets name 2
      let fun = closedTerm "\\(y :: Int) (b :: Type) -> y"
      -- a gets the name of b
      withTestScope [atRaw 2 (tyVarD "a"), atRaw 5 (varD "x" "a")] $ \ts -> do
        let res = betaTm (testScope ts) (App (sinkClosed fun) (termIn ts "x"))
        assertLintClean ts res
        case res of
          TyLam tb (Var _ (VarTy _ _ an) _) -> do
            assertBool "renamed" (tyNameRaw (tyNameOf tb) /= 2)
            -- The type of x is the free a
            tyNameRaw an @?= 2
          _ -> assertFailure "unexpected shape"

    -- Substituting @[b := a]@ in @forall a. b@ must rename the binder @a@.
    -- https://github.com/clash-lang/clash-compiler/pull/361
  , testCase "substitution renames a forall binder that would capture (#361)" $
      -- In the empty scope, b gets name 1 and a gets name 2
      withTestScope [atRaw 2 (tyVarD "a")] $ \ts ->
        case sinkClosed (closedType "forall (b :: Type). forall (a :: Type). b") of
          ForAllTy bb body -> do
            let sc = testScope ts
            assertTypeIs ts "forall (c :: Type). a"
              (substTy (extendTySubst (idSubst sc) bb (typeIn ts "a")) body)
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

  , testCase "substitution keeps binders that don't clash" $
      -- x gets name 1, y name 2 and z name 3
      withTerm "(\\(y :: Int) (z :: Int) -> y) (x :: Int)" $ \ts -> \case
        app@(App (Lam _ (Lam bz _)) _) -> case betaTm (testScope ts) app of
          Lam bz' _ -> tmNameRaw (tmNameOf bz') @?= tmNameRaw (tmNameOf bz)
          _ -> assertFailure "unexpected shape"
        _ -> assertFailure "unexpected shape"

    -- @(/\a -> \(x :: a) -> x) \@Int@: the type of the occurrence of @x@
    -- changes along with the type of its binder
  , testCase "substitution updates the types of occurrences" $ do
      let fun = closedTerm "\\(a :: Type) (x :: a) -> x"
          res = betaTy emptyScope (TyApp fun (closedType "Int"))
      case res of
        Lam b (Var _ ty _) -> do
          assertTypeIs emptyTestScope "Int" (idBinderType b)
          maybe (assertFailure "the type mentions x") (assertTypeIs emptyTestScope "Int")
            (unsinkType (idBinderNameBinders b) ty)
        _ -> assertFailure "unexpected shape"

    -- An occurrence whose type mentions a variable that the type of its
    -- binder doesn't, as one could build by hand. Substitution must not leave
    -- a reference to the substituted variable.
  , testCase "substitution substitutes in the types of all occurrences" $
      withTestScope [varD "y" "Int"] $ \ts -> case termIn ts "y" of
        Var occ _ yn -> do
          let sc = testScope ts
              occA = C.mkUnsafeSystemName "a" 0
              -- /\a -> (y :: a)
              tyLam = withFreshTyBinder sc occA P.liftedTypeKind $ \_ ba ->
                TyLam ba (Var occ (tyBinderVar ba) (sink yn))
          case betaTy sc (TyApp tyLam intTy) of
            Var _ ty _ -> do
              assertBool "no free type variables" (nullNameSet (freeTyVarsOfType ty))
              assertTypeIs ts "Int" ty
            _ -> assertFailure "unexpected shape"
        _ -> assertFailure "unexpected shape"

    -- @(/\a. case v of MkE \@b (y :: b) -> g \@a) \@b@, where the existential
    -- type variable of the pattern has the same name as the free type
    -- variable @b@
  , testCase "existential type variables don't capture type arguments" $
      -- g gets name 1 and v name 2; a gets 3 and the existential 4
      withTestScope [varD "g" "forall (a :: Type). a", varD "v" "E"] $ \ts0 -> do
        let fun0 = termIn ts0 "\\(a :: Type) -> (case v of { MkE (b :: Type) (y :: b) -> g @a }) :: a"
        extendTestScope ts0 [atRaw 4 (tyVarD "c")] $ \ts1 -> do
          let res = betaTy (testScope ts1) (TyApp (sink fun0) (typeIn ts1 "c"))
          assertTermIs ts1 "(case v of { MkE (b :: Type) (y :: b) -> g @c }) :: c" res
          assertLintClean ts1 res

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
      case closedTerm "\\(x :: Int) (y :: Int) -> x" of
        Lam _ (Lam by e) -> assertBool "y doesn't occur" (isJust (unsinkIdBinder by e))
        _ -> assertFailure "unexpected shape"
      case closedTerm "\\(x :: Int) (y :: Int) -> y" of
        Lam _ (Lam by e) -> assertBool "y occurs" (isNothing (unsinkIdBinder by e))
        _ -> assertFailure "unexpected shape"

  , testCase "teleIdBinderTypes" $
      -- @letrec (x :: Int) = 1; (y :: a) = y in x@, with a free type variable
      -- @a@
      withTestScope [tyVarD "a"] $ \ts ->
        case termIn ts "let { (x :: Int) = 1; (y :: a) = y } in x" of
          Let (Rec bs _) _ -> case teleIdBinderTypes bs of
            [t1, t2] -> do
              assertTypeIs ts "Int" t1
              assertTypeIs ts "a" t2
            tys -> assertFailure ("unexpected number of types: " <> show (length tys))
          _ -> assertFailure "unexpected shape"

    -- The existential type variable only occurs in the type of an occurrence
    -- of a variable from outside the alternative, as one could build by hand
  , testCase "unsinkAlt looks at the types of occurrences" $
      withTestScope [varD "v" "E", varD "w" "Int"] $ \ts ->
        case (termIn ts "w", termIn ts "(case v of { MkE (b :: Type) (y :: b) -> y }) :: E") of
          (wVar@(Var occW _ wn), Case _ _ [alt]) -> openAlt (testScope ts) alt $ \_ p e -> case e of
            Var _ exTy _ -> do
              assertBool "w occurs at the existential type"
                (isNothing (unsinkAlt p (Var occW exTy (sink wn))))
              assertBool "w occurs at its own type"
                (isJust (unsinkAlt p (sink wVar)))
              assertBool "the existential occurs"
                (isNothing (strengthenTyPat p exTy))
              assertBool "Int doesn't mention the existential"
                (isJust (strengthenTyPat p intTy))
            _ -> assertFailure "unexpected shape"
          _ -> assertFailure "unexpected shape"

    -- Code that builds terms, e.g. primitive reductions, creates fresh binders
    -- in a pure context
  , testCase "closed terms can be built with fresh binders" $ do
      let occ = C.mkUnsafeSystemName "x" 0
          tm = withFreshIdBinder emptyScope occ intTy $ \_ b -> Lam b (idBinderVar b)
      assertTermIs emptyTestScope "\\(x :: Int) -> x" tm

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

  , testCase "extendTmSubstTele needs as many terms as binders" $
      case closedTerm "let { (x :: Int) = x; (y :: Int) = x } in y" of
        Let (Rec bs _) _ -> do
          let s = idSubst emptyScope
              lit = Literal (IntLiteral 1)
          assertBool "too few" (isNothing (extendTmSubstTele s bs [lit]))
          assertBool "exactly" (isJust (extendTmSubstTele s bs [lit, lit]))
          assertBool "too many" (isNothing (extendTmSubstTele s bs [lit, lit, lit]))
        _ -> assertFailure "unexpected shape"

  , testCase "withFreshIdBinders" $
      withTestScope [atRaw 7 (varD "x" "Int")] $ \ts -> do
        let sc = testScope ts
            occ = C.mkUnsafeSystemName "f" 0
        withFreshIdBinders sc [(occ, intTy), (occ, intTy)] $ \_ bs vars -> case (bs, vars) of
          (TCons b1 (TCons b2 TNil), [Var _ _ n1, Var _ _ n2]) -> do
            [tmNameRaw (tmNameOf b1), tmNameRaw (tmNameOf b2)] @?= [8, 9]
            [tmNameRaw n1, tmNameRaw n2] @?= [8, 9]
          _ -> assertFailure "unexpected shape"

  , withTests 500 $ testGroup "properties"
      [ testProperty "substitution agrees with substitution without renaming" $
          forAllShrink genRedex shrinkRedex betaAgrees

      , testProperty "substitution for variables of the scope agrees with substitution without renaming" $
          forAllShrink genSunk shrinkSunk $ \(Sunk ts0 ts1 tm) -> case IntMap.elems (testVars ts0) of
            [] -> property True
            x0 : _ -> case sinkInto (testScope ts1) x0 of
              Var _ _ xn -> do
                let sc = testScope ts1
                    lit = Literal (IntLiteral 1)
                    expected = naiveSubst IntMap.empty (IntMap.singleton (tmNameRaw xn) lit)
                                 (freshenDistinct sc (sink tm))
                forAllScrambled sc (sink tm) $ \tm1 ->
                  resultAgrees ts1 expected
                    (substTm (tmVarsSubst sc (singletonNameMap xn lit)) tm1)
              _ -> counterexample "not a variable" False

      , testProperty "substitution for the variables of a pattern agrees with substitution without renaming" $
          forAllShrink genKnownCon shrinkKnownCon knownConAgrees

        -- A binder in a value may have the name of a variable in scope, of
        -- either sort, if that doesn't occur in its scope: e.g. that of a
        -- binder created for the value. Substitution must not substitute for
        -- it.
      , testProperty "substitution keeps binders that shadow the substituted one" $
          forAllShrink genInScope shrinkInScope $ \(InScope ts body) -> do
            let sc = testScope ts
                -- The binders get the name just above the watermark of the
                -- scope, like the first binder of the body
                lam = withFreshIdBinder sc (C.mkUnsafeSystemName "x" 0) intTy $ \_ b ->
                        Lam b (sink body)
                tyLam = withFreshTyBinder sc (C.mkUnsafeSystemName "a" 0) P.liftedTypeKind $ \_ b ->
                          TyLam b (sink body)
            resultAgrees ts body (betaTm sc (App lam (Literal (IntLiteral 1)))) .&&.
              resultAgrees ts body (betaTy sc (TyApp tyLam intTy))

        -- Every binder gets the name just above the watermark of the scope at
        -- it: binders in different subterms may get the same name.
      , testProperty "freshen is alpha-equivalent and renames binders just above the watermark" $
          forAllShrink genSunk shrinkSunk $ \(Sunk _ ts1 tm) -> do
            let sc = testScope ts1
            forAllScrambled sc (sink tm) $ \tm1 -> do
              let tm2 = freshen sc tm1
              counterexample ("freshened:\n" <> showT tm2) $
                counterexample "aeqTm" (aeqTm tm1 tm2) .&&.
                counterexample "binders just above the watermark"
                  (bindersNot (\w k -> k == w + 1) (scopeWatermark sc) tm2 === []) .&&.
                lintTerm (testLintScope ts1) tm2 === []

        -- Every binder gets a different name, numbered in depth-first order
      , testProperty "freshenDistinct is alpha-equivalent and numbers binders in order" $
          forAllShrink genSunk shrinkSunk $ \(Sunk _ ts1 tm) -> do
            let sc = testScope ts1
                w = scopeWatermark sc
            forAllScrambled sc (sink tm) $ \tm1 -> do
              let tm2 = freshenDistinct sc tm1
              counterexample ("freshened:\n" <> showT tm2) $
                counterexample "aeqTm" (aeqTm tm1 tm2) .&&.
                counterexample "binders numbered from the watermark"
                  (binderRaws tm2 === [w + 1 .. w + length (binderRaws tm1)]) .&&.
                lintTerm (testLintScope ts1) tm2 === []

      , testProperty "freshenDistinct is idempotent" $
          forAllShrink genInScope shrinkInScope $ \(InScope ts tm) -> do
            let sc = testScope ts
                tm1 = freshenDistinct sc tm
            show (freshenDistinct sc tm1) === show tm1

        -- See "hasFreeVars agrees with free variables"
      , testProperty "closeTerm succeeds exactly when a term has no free variables" $
          forAll arbitrary $ \closed0 ->
            forAllShrink (if closed0 then genClosedSunk else genSunk) shrinkSunk $ \(Sunk _ ts1 tm) -> do
              let sc = testScope ts1
              forAllScrambled sc (sink tm) $ \tm1 -> do
                let closed = null (refFreeTmVars tm1) && null (refFreeTyVars tm1)
                cover 20 closed "no free variables" $ case closeTerm tm1 of
                  Nothing -> counterexample "closeTerm failed" (not closed)
                  Just tm2 -> counterexample "closeTerm succeeded" closed .&&.
                    show (atScope sc (sinkClosed tm2)) === show tm1 .&&.
                    lintTerm (testLintScope emptyTestScope) tm2 === []

      , testProperty "closeType succeeds exactly when a type has no free variables" $
          forAllBlind genTestScope $ \(SomeScope ts) ->
            forAll arbitrary $ \closed0 ->
              forAllShow
                (if closed0
                   then sinkClosed <$> genType (ctxOf emptyTestScope) P.liftedTypeKind 3
                   else genType (ctxOf ts) P.liftedTypeKind 3)
                showTy $ \ty1 -> do
                  let closed = nullNameSet (freeTyVarsOfType ty1)
                  cover 20 closed "no free variables" $ case closeType ty1 of
                    Nothing -> counterexample "closeType failed" (not closed)
                    Just ty2 -> counterexample "closeType succeeded" closed .&&.
                      show (atScope (testScope ts) (sinkClosed ty2)) === show ty1

      , testProperty "opening binders is alpha-equivalent and renames binders that clash" $
          forAllShrink genSunk shrinkSunk $ \(Sunk _ ts1 tm) -> do
            let sc = testScope ts1
            forAllScrambled sc (sink tm) $ \tm1 -> do
              let (problems, tm2) = reopenTerm sc tm1
              counterexample ("reopened:\n" <> showT tm2) $
                problems === [] .&&.
                counterexample "aeqTm" (aeqTm tm1 tm2) .&&.
                lintTerm (testLintScope ts1) tm2 === []

        -- 'freshenDistinct' numbers binders such that they never clash
      , testProperty "opening the binders of a freshened term keeps them" $
          forAllShrink genInScope shrinkInScope $ \(InScope ts tm0) -> do
            let tm = freshenDistinct (testScope ts) tm0
                (problems, tm2) = reopenTerm (testScope ts) tm
            problems === [] .&&. binderRaws tm2 === binderRaws tm .&&.
              property (aeqTm tm tm2)
      ]
  , testGroup "closing over variables" closeOverTests
  , withTests 500 $ testGroup "closing over variables, properties"
      [ testProperty "freeVarOccs gives every free variable once, in an order independent of binders" $
          forAllShrink genSunk shrinkSunk $ \(Sunk _ ts1 tm) -> do
            let sc = testScope ts1
                (tys0, tms0) = freeVarOccs (sinkInto sc tm)
            forAllScrambled sc (sink tm) $ \tm1 ->
              let (tys, tms) = freeVarOccs tm1
              in  counterexample "type variables" (List.sort (map tyOccRaw tys) === refFreeTyVars tm1) .&&.
                    counterexample "term variables" (List.sort (map tmOccRaw tms) === refFreeTmVars tm1) .&&.
                    counterexample "order" ((map tyOccRaw tys, map tmOccRaw tms) === (map tyOccRaw tys0, map tmOccRaw tms0))

        -- Abstracting over the free variables, and applying the result to
        -- them gives back the term
      , testProperty "abstracting over the free variables and applying the result to them" $
          forAllShrink genInScope shrinkInScope $ \(InScope ts tm) -> do
            let sc = testScope ts
                (tys0, tms) = freeVarOccs tm
                -- Type variables after the ones in their kinds
                tys = List.sortOn tyOccRaw tys0
            case closeOver tys tms (\_ tvTele idTele cl -> (\t -> mkTyLams (mkLams t idTele) tvTele) <$> closureTerm cl tm) of
              Just (Just closed) ->
                let res = applyAll sc (sinkClosed closed) (map Right tys <> map Left tms)
                in  counterexample ("closed:\n" <> showT closed <> "\napplied:\n" <> showT res) $
                      lintTerm (testLintScope emptyTestScope) closed === [] .&&.
                        property (aeqTm tm res)
              _ -> counterexample "closeOver failed" False
      ]
  , testCase "teleIdVars gives the variables of a telescope in its scope" $
      case closedTerm "let { (x :: Int) = y; (y :: Int) = x } in x" of
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

  , testCase "patVars gives the variables of a pattern in its scope" $
      withTestScope [varD "v" "E"] $ \ts ->
        case termIn ts "(case v of { MkE (b :: Type) (y :: b) -> y }) :: E" of
          Case _ _ [alt] -> openAlt (testScope ts) alt $ \_ p e -> case patVars p of
            ([VarTy _ _ tn], [yVar@(Var _ yTy _)]) -> do
              assertBool "the variable of the field" (aeqTm yVar e)
              -- The type of the field is the existential type variable
              case yTy of
                VarTy _ _ tn' -> tyNameRaw tn' @?= tyNameRaw tn
                _ -> assertFailure "unexpected type"
            _ -> assertFailure "unexpected variables"
          _ -> assertFailure "unexpected shape"
  ]

-- | Apply a term to types and terms, and beta-reduce
applyAll :: Distinct n => Scope n -> Term n -> [Either (Term n) (Type n)] -> Term n
applyAll sc = foldl step
 where
  step e = \case
    Left a -> betaTm sc (App e a)
    Right ty -> betaTy sc (TyApp e ty)

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

closeOverTests :: [TestTree]
closeOverTests =
  [ testCase "binds the variables with their names, kinds and types" $
      withTestScope decls $ \ts -> do
        let res = closeOver [tyOcc ts "k", tyOcc ts "a"] [termIn ts "x"] $ \_ tvTele idTele _ ->
              case (tvTele, idTele) of
                (TCons bk (TCons ba TNil), TCons bx TNil) ->
                  [ tyNameRaw (tyNameOf bk), tyNameRaw (tyNameOf ba), tmNameRaw (tmNameOf bx) ]
                    == [1, 2, 3]
                    && rawTyVar (tyBinderKind ba) == Just 1
                    && rawTyVar (idBinderType bx) == Just 2
                _ -> False
        res @?= Just True
  , testCase "closes terms and types that only mention the variables" $
      withTestScope decls $ \ts -> do
        let res = closeOver [tyOcc ts "k", tyOcc ts "a"] [termIn ts "x"] $ \_ _ _ cl ->
              ( isJust (closureTerm cl (termIn ts "x"))
              , isNothing (closureTerm cl (termIn ts "y"))
              , isJust (closureType cl (typeIn ts "a"))
              )
        res @?= Just (True, True, True)
  , testCase "rejects a term whose types mention other type variables" $
      withTestScope decls $ \ts -> do
        let res = closeOver [tyOcc ts "k"] [] $ \_ _ _ cl ->
              isNothing (closureTerm cl (termIn ts "x"))
        res @?= Just True
  , testCase "rejects a type variable before the type variables in its kind" $
      withTestScope decls $ \ts ->
        assertBool "Nothing" (isNothing (closeOver [tyOcc ts "a", tyOcc ts "k"] [] (\_ _ _ _ -> ())))
  , testCase "rejects a term variable whose type mentions other type variables" $
      withTestScope decls $ \ts ->
        assertBool "Nothing" (isNothing (closeOver [tyOcc ts "k"] [termIn ts "x"] (\_ _ _ _ -> ())))
  , testCase "rejects a variable given twice" $
      withTestScope decls $ \ts ->
        assertBool "Nothing" (isNothing (closeOver [] [termIn ts "y", termIn ts "y"] (\_ _ _ _ -> ())))
  , testCase "freeVarOccs gives the occurrences of free variables, not bound ones" $
      withTestScope decls $ \ts -> do
        let (tys, tms) = freeVarOccs (termIn ts "\\(z :: a) -> x y z")
        (map tyOccRaw tys, map tmOccRaw tms) @?= ([2, 1], [3, 4])
  ]
 where
  -- k gets name 1, a name 2, x name 3, and y name 4
  decls = [tyVarD "k", kindedTyVarD "a" "k", varD "x" "a", varD "y" "Int"]

  tyOcc :: Distinct n => TestScope n -> TestType -> Type n
  tyOcc = typeIn

  rawTyVar :: Type n -> Maybe Int
  rawTyVar = \case
    VarTy _ _ n -> Just (tyNameRaw n)
    _ -> Nothing
