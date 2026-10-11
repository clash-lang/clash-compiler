{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities to write unit tests on scoped Clash Core ("Clash.Core.Scoped").

  Most tests write their inputs as Clash Core, e.g. with
  'Test.Clash.Rewrite.parseToTermQQ', and convert them. Note that conversion
  deshadows, so a converted input never has binders that clash with the
  scope. To test how a function handles such binders, convert the part of the
  input that has them in a smaller scope, and 'sink' it into a scope with more
  variables: e.g. into the scope of 'extendSunk', which contains 'sunkVar',
  whose unique is above the names conversion gave the binders. Sinking allows
  binders to clash, so this is the situation that functions going under
  binders must handle: they have to rename them.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Clash.Scoped
  ( -- * Converting
    withFreeVarsOf
  , withFreeVars
  , extendFreeVars
  , abstractFreeVars
  , sinkInto
  , atScope
  , roundTrip
  , extendSunk
  , sunkVar
  , withFreeTyVarsOf
  , withTypeScopes
  , withTermScopes
  , mkScopedBindingMap
  , testTypeEnv
    -- * Assertions
  , assertScopedAlphaEq
  , assertAgreesTm
  , assertAgreesTy
  , assertScopedTyAlphaEq
  , assertScopedMaybeTyAlphaEq
  , nameSetRaws
    -- * Beta reduction
  , betaTm
  , betaTy
    -- * Invariants
  , LintScope
  , lintScopeOf
  , lintTerm
  , lowBinders
  , bindersNot
  , binderRaws
  , reopenTerm
  , scrambleBinders
  , forAllScrambled
    -- * Comparing Clash Core
  , aeqTermTyped
  , stripCoercionTypes
    -- * Properties
  , withTests
    -- * Running scoped transformations
  , runScopedClosed
  , runScopedOpen
  , runViaFoil
  , runScopedOn
  , assertLetHandlerAgrees
  , assertRewritesAgree
  , assertRewritesAgreeUpTo
  , assertNewBindingsAgree
  , Step (..)
  , atPath
  , somePrim
    -- * Evaluating
  , scopedWhnf
  ) where

import qualified Control.Lens as Lens
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, state)
import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Data.Default (def)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack)
import Test.Tasty (TestTree, adjustOption)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)
import Test.Tasty.QuickCheck
  (Gen, Property, QuickCheckTests (..), Testable, choose, forAllShow, frequency)

import qualified Clash.Core.FreeVars as C
import qualified Clash.Core.Literal as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C
import Clash.Core.VarEnv (unionInScope)
import Clash.Unique (Unique)

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.FreeVars (HasNameBinder (..), freeTmVars, freeTyVars, freeTyVarsOfType)
import Clash.Core.Scoped.Internal.Names
  ( TmName (UnsafeTmName), TmNameBinder (UnsafeTmNameBinder), TyName (UnsafeTyName)
  , TyNameBinder (UnsafeTyNameBinder))
import Clash.Core.Scoped.Internal.Scope
  (Name (UnsafeName), NameBinder (UnsafeNameBinder), nameId, nameOf, scopeWatermark)
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TypeEnv (TypeEnv, mkTypeEnv)
import Clash.Rewrite.Scoped
  ( ScopedRewrite, Frame (LetBinding), pushFrame, rootContext, runScopedTransform
  , underAlt, underLam, underLet, underLetRec, underTyLam)
import qualified Clash.Rewrite.Scoped as Scoped
import Clash.Rewrite.Scoped.ViaFoil (viaFoil)
import Clash.Rewrite.Types
  (Rewrite, RewriteEnv, RewriteMonad, RewriteState (..), TransformContext)
import qualified Clash.Rewrite.Types as C

import Clash.Driver.Types (Binding (..))
import qualified Clash.Data.UniqMap as UniqMap

import Test.Clash.Differential (KnownDifference, Outcome (..))
import Test.Clash.Differential.Sites (compareAtSite)
import Test.Clash.Rewrite
  ( assertAlphaEq, globalsByName, inScopeOf, intId, mkBinding, runRewriteTest
  , testTyConMap)
import Test.Clash.Scoped.Evaluator (scopedWhnf)
import Test.Clash.Scoped.Gen (FreeVars (..), showTerm)

-- | A conversion environment with the free variables of the given Clash terms
-- (both type and term variables), which keep their uniques. The type
-- variables come after the type variables in their kinds.
withFreeVarsOf :: [C.Term] -> (forall n. Distinct n => ConvEnv n -> r) -> r
withFreeVarsOf tms = withConvEnv tvs ids
 where
  tvs = C.dependencySortTyVars (List.nubBy sameUniq (concatMap (Lens.toListOf C.termFreeTyVars) tms))
  ids = List.nubBy sameUniq (concatMap (Lens.toListOf C.freeLocalIds) tms)

  sameUniq :: C.Var a -> C.Var a -> Bool
  sameUniq v1 v2 = C.varUniq v1 == C.varUniq v2

-- | A conversion environment with the free type variables of the given Clash
-- types, which keep their uniques
withFreeTyVarsOf :: [C.Type] -> (forall n. Distinct n => ConvEnv n -> r) -> r
withFreeTyVarsOf tys = withConvEnv tvs []
 where
  tvs = List.nubBy sameUniq (concatMap (Lens.toListOf C.typeFreeVars) tys)

  sameUniq :: C.TyVar -> C.TyVar -> Bool
  sameUniq v1 v2 = C.varUniq v1 == C.varUniq v2

-- | Run an assertion on a Clash type converted to scoped Core, twice: in the
-- scope of its free type variables, and sunk into a larger scope (see
-- 'extendSunk'), where all of its binders clash with the scope. So a function
-- that goes under the binders of the type must rename them in the second run.
withTypeScopes
  :: C.Type
  -> (forall n. Distinct n => Scope n -> Type n -> Assertion)
  -> Assertion
withTypeScopes ty k = withFreeTyVarsOf [ty] $ \env -> do
  let ty' = fromClashType env ty
  k (convScope env) ty'
  extendSunk env [] [] $ \env' -> k (convScope env') (sink ty')

-- | 'withTypeScopes' for terms: run an assertion on a Clash term converted to
-- scoped Core, in the scope of its free variables, and sunk into a larger
-- scope where all of its binders clash with the scope
withTermScopes
  :: C.Term
  -> (forall n. Distinct n => Scope n -> Term n -> Assertion)
  -> Assertion
withTermScopes tm k = withFreeVarsOf [tm] $ \env -> do
  let tm' = fromClashTerm env tm
  k (convScope env) tm'
  extendSunk env [] [] $ \env' -> k (convScope env') (sink tm')

-- | Convert a Clash term to scoped Core and back, in the scope of its free
-- variables
roundTrip :: HasCallStack => C.Term -> C.Term
roundTrip tm = withFreeVarsOf [tm] $ \env -> toClashTerm (fromClashTerm env tm)

-- | The environment of the types of 'testTyConMap'
testTypeEnv :: TypeEnv
testTypeEnv = mkTypeEnv testTyConMap

-- | Abstract a term over free variables: type lambdas for the type variables,
-- and lambdas for the term variables inside those. So the result has no free
-- variables if the term only has the given ones.
abstractFreeVars :: FreeVars -> C.Term -> C.Term
abstractFreeVars (FreeVars tvs ids) tm = foldr C.TyLam (foldr C.Lam tm ids) tvs

-- | Global bindings of the given closed terms, converted to scoped Core. See
-- 'Test.Clash.Rewrite.mkBindingMap'.
mkScopedBindingMap :: HasCallStack => [(C.Id, C.Term)] -> GlobalEnv (Binding (Term 'VoidS))
mkScopedBindingMap bs =
  globalEnvFromList [(mkGlobalId i, fromClashTermClosed <$> mkBinding i t) | (i, t) <- bs]

-- | A variable whose unique is above the names conversion gives the binders of
-- the inputs of the tests, see 'extendSunk'
sunkVar :: C.Id
sunkVar = intId C.System "sunk" 1000000

-- | Extend a conversion environment with 'sunkVar' and the given variables.
-- A value converted in the original environment can be 'sink'ed into the
-- extended one, where all of its binders may clash with the scope: those of the
-- extended scope are at most the unique of 'sunkVar'.
extendSunk
  :: Distinct n
  => ConvEnv n
  -> [C.TyVar]
  -> [C.Id]
  -> (forall l. DExt n l => ConvEnv l -> r)
  -> r
extendSunk env tvs ids = extendConvEnv env tvs (sunkVar : ids)

-- | Assert that a scoped term, converted to Clash, is alpha-equivalent to a
-- Clash term
assertScopedAlphaEq :: HasCallStack => C.Term -> Term n -> Assertion
assertScopedAlphaEq expected actual = assertAlphaEq expected (toClashTerm actual)

-- | Assert that a function on scoped terms agrees with one on Clash terms:
-- applied to a Clash term converted to scoped Core, in the scope of its free
-- variables, and converted back, it gives a term alpha-equivalent to the one
-- the function on Clash terms gives.
assertAgreesTm
  :: HasCallStack
  => (C.Term -> C.Term)
  -> (forall n. Distinct n => Scope n -> Term n -> Term n)
  -> C.Term
  -> Assertion
assertAgreesTm f g tm =
  withFreeVarsOf [tm] $ \env ->
    assertScopedAlphaEq (f tm) (g (convScope env) (fromClashTerm env tm))

-- | The type version of 'assertAgreesTm'
assertAgreesTy
  :: HasCallStack
  => (C.Type -> C.Type)
  -> (forall n. Distinct n => Scope n -> Type n -> Type n)
  -> C.Type
  -> Assertion
assertAgreesTy f g ty =
  withFreeTyVarsOf [ty] $ \env ->
    assertScopedTyAlphaEq (f ty) (g (convScope env) (fromClashType env ty))

-- | Assert that a scoped type, converted to Clash, is alpha-equivalent to a
-- Clash type
assertScopedTyAlphaEq :: HasCallStack => C.Type -> Type n -> Assertion
assertScopedTyAlphaEq expected actual = assertAlphaEq expected (toClashType actual)

-- | 'assertScopedTyAlphaEq' for optional types
assertScopedMaybeTyAlphaEq :: HasCallStack => Maybe C.Type -> Maybe (Type n) -> Assertion
assertScopedMaybeTyAlphaEq expected actual = case (expected, actual) of
  (Just e, Just a) -> assertScopedTyAlphaEq e a
  (Nothing, Nothing) -> pure ()
  (Just e, Nothing) -> assertFailure ("expected Just " <> show e <> ", got Nothing")
  (Nothing, Just a) -> assertFailure ("expected Nothing, got Just " <> show (toClashType a))

-- | The raw names in a set, in ascending order
nameSetRaws :: ScopedName k => NameSet k n -> [Int]
nameSetRaws = map scopedNameRaw . nameSetToList

-- | Beta-reduce an application of a lambda. Errors on other terms.
betaTm :: (HasCallStack, Distinct n) => Scope n -> Term n -> Term n
betaTm sc = \case
  App (Lam b e) arg -> substTm (extendTmSubst (idSubst sc) b arg) e
  _ -> error "betaTm: not an application of a lambda"

-- | Beta-reduce a type application of a type lambda. Errors on other terms.
betaTy :: (HasCallStack, Distinct n) => Scope n -> Term n -> Term n
betaTy sc = \case
  TyApp (TyLam b e) ty -> substTm (extendTySubst (idSubst sc) b ty) e
  _ -> error "betaTy: not a type application of a type lambda"

-- * Generated free variables

-- | A conversion environment with generated free variables, which keep their
-- uniques: these are all different
withFreeVars :: FreeVars -> (forall n. Distinct n => ConvEnv n -> r) -> r
withFreeVars (FreeVars tvs ids) = withConvEnv tvs ids

-- | Extend a conversion environment with more generated free variables, see
-- 'Test.Clash.Scoped.Gen.genMoreFreeVars'
extendFreeVars
  :: Distinct n
  => ConvEnv n
  -> FreeVars
  -> (forall l. DExt n l => ConvEnv l -> r)
  -> r
extendFreeVars env (FreeVars tvs ids) = extendConvEnv env tvs ids

-- | 'sink' into the given scope
sinkInto :: (Sinkable e, DExt n l) => Scope l -> e n -> e l
sinkInto _ = sink

-- | A value in the scope of the given scope, e.g. to fix the scope of
-- 'sinkClosed'
atScope :: Scope n -> e n -> e n
atScope _ e = e

-- * Invariants

-- | The raw names of the type and of the term variables in scope, for
-- 'lintTerm'
data LintScope = LintScope !IntSet !IntSet

-- | The names of the variables of a conversion environment, given its free
-- variables
lintScopeOf :: Distinct n => ConvEnv n -> FreeVars -> LintScope
lintScopeOf env (FreeVars tvs ids) =
  LintScope
    (IntSet.fromList [tyNameRaw n | VarTy _ _ n <- map (fromClashType env . C.VarTy) tvs])
    (IntSet.fromList [tmNameRaw n | Var _ _ n <- map (fromClashTerm env . C.Var) ids])

bindLintTy :: Int -> LintScope -> LintScope
bindLintTy k (LintScope tys tms) = LintScope (IntSet.insert k tys) (IntSet.delete k tms)

bindLintTm :: Int -> LintScope -> LintScope
bindLintTm k (LintScope tys tms) = LintScope (IntSet.delete k tys) (IntSet.insert k tms)

-- | The raw name a binder binds
binderRaw :: HasNameBinder b => b n l -> Int
binderRaw = nameId . nameOf . nameBinderOf

-- | Problems with the names of a term in a scope with the given variables:
-- occurrences of names that are not in scope, or that are not of their sort.
-- A binder makes a name of the other sort inaccessible. Looks at all types,
-- including those of variable occurrences.
lintTerm :: LintScope -> Term n -> [String]
lintTerm ls0 = goTm ls0
 where
  goTy :: LintScope -> Type x -> [String]
  goTy ls@(LintScope tys _) = \case
    VarTy _ k n ->
      ["type variable " <> show (tyNameRaw n) <> " not in scope" | not (IntSet.member (tyNameRaw n) tys)]
        <> goTy ls k
    ForAllTy b t -> goTy ls (tyBinderKind b) <> goTy (bindLintTy (binderRaw b) ls) t
    AppTy t1 t2 -> goTy ls t1 <> goTy ls t2
    AnnType _ t -> goTy ls t
    ConstTy {} -> []
    LitTy {} -> []

  goTm :: LintScope -> Term x -> [String]
  goTm ls@(LintScope _ tms) = \case
    Var _ ty n ->
      ["term variable " <> show (tmNameRaw n) <> " not in scope" | not (IntSet.member (tmNameRaw n) tms)]
        <> goTy ls ty
    GlobalVar {} -> []
    Data {} -> []
    Literal {} -> []
    Prim {} -> []
    Lam b e -> goTy ls (idBinderType b) <> goTm (bindLintTm (binderRaw b) ls) e
    TyLam b e -> goTy ls (tyBinderKind b) <> goTm (bindLintTy (binderRaw b) ls) e
    App e1 e2 -> goTm ls e1 <> goTm ls e2
    TyApp e ty -> goTm ls e <> goTy ls ty
    Let (NonRec b x) e ->
      goTy ls (idBinderType b) <> goTm ls x <> goTm (bindLintTm (binderRaw b) ls) e
    Let (Rec bs xs) e -> goIdTele ls bs (\ls' -> concatMap (goTm ls') (e:xs))
    Case e ty alts -> goTm ls e <> goTy ls ty <> concatMap (goAlt ls) alts
    Cast e t1 t2 -> goTm ls e <> goTy ls t1 <> goTy ls t2
    Tick t e -> goTick ls t <> goTm ls e

  goAlt :: LintScope -> Alt x -> [String]
  goAlt ls (Alt p e) = case p of
    DataPat _ tvs ids -> goTyTele ls tvs (\ls1 -> goIdTele ls1 ids (\ls2 -> goTm ls2 e))
    LitPat _ -> goTm ls e
    DefaultPat -> goTm ls e

  goTick :: LintScope -> TickInfo x -> [String]
  goTick ls = \case
    NameMod _ ty -> goTy ls ty
    Attributes ty e -> goTy ls ty <> goTm ls e
    _ -> []

  goIdTele :: LintScope -> Tele IdBinder x y -> (LintScope -> [String]) -> [String]
  goIdTele ls TNil k = k ls
  goIdTele ls (TCons b bs) k = goTy ls (idBinderType b) <> goIdTele (bindLintTm (binderRaw b) ls) bs k

  goTyTele :: LintScope -> Tele TyBinder x y -> (LintScope -> [String]) -> [String]
  goTyTele ls TNil k = k ls
  goTyTele ls (TCons b bs) k = goTy ls (tyBinderKind b) <> goTyTele (bindLintTy (binderRaw b) ls) bs k

-- | The binders of a term that are not above the watermark of the scope at
-- them, given the watermark of the scope of the term. Includes the binders of
-- the types in the term, except those of the types of variable occurrences,
-- which are those of their binders.
--
-- Values converted from Clash Core and 'freshen'ed ones have no such binders.
-- Others may: e.g. a value 'sink'ed into a larger scope.
lowBinders :: Int -> Term n -> [Int]
lowBinders = bindersNot (<)

-- | The binders of a term whose names don't satisfy the predicate, given the
-- watermark of the scope at them, and that of the scope of the term. See
-- 'lowBinders'.
bindersNot :: (Int -> Int -> Bool) -> Int -> Term n -> [Int]
bindersNot ok = goTm
 where
  check :: Int -> Int -> [Int]
  check w k = [k | not (ok w k)]

  goTy :: Int -> Type x -> [Int]
  goTy w = \case
    VarTy {} -> []
    ForAllTy b t -> goTy w (tyBinderKind b) <> check w (binderRaw b) <> goTy (max w (binderRaw b)) t
    AppTy t1 t2 -> goTy w t1 <> goTy w t2
    AnnType _ t -> goTy w t
    ConstTy {} -> []
    LitTy {} -> []

  goTm :: Int -> Term x -> [Int]
  goTm w = \case
    Var {} -> []
    GlobalVar {} -> []
    Data {} -> []
    Literal {} -> []
    Prim {} -> []
    Lam b e -> goTy w (idBinderType b) <> check w (binderRaw b) <> goTm (max w (binderRaw b)) e
    TyLam b e -> goTy w (tyBinderKind b) <> check w (binderRaw b) <> goTm (max w (binderRaw b)) e
    App e1 e2 -> goTm w e1 <> goTm w e2
    TyApp e ty -> goTm w e <> goTy w ty
    Let (NonRec b x) e ->
      goTm w x <> goTy w (idBinderType b) <> check w (binderRaw b) <> goTm (max w (binderRaw b)) e
    Let (Rec bs xs) e -> goTele idBinderType bs w (\w' -> concatMap (goTm w') (e:xs))
    Case e ty alts -> goTm w e <> goTy w ty <> concatMap (goAlt w) alts
    Cast e t1 t2 -> goTm w e <> goTy w t1 <> goTy w t2
    Tick t e -> goTick w t <> goTm w e

  goAlt :: Int -> Alt x -> [Int]
  goAlt w (Alt p e) = case p of
    DataPat _ tvs ids -> goTele tyBinderKind tvs w (\w1 -> goTele idBinderType ids w1 (\w2 -> goTm w2 e))
    LitPat _ -> goTm w e
    DefaultPat -> goTm w e

  goTick :: Int -> TickInfo x -> [Int]
  goTick w = \case
    NameMod _ ty -> goTy w ty
    Attributes ty e -> goTy w ty <> goTm w e
    _ -> []

  goTele
    :: HasNameBinder b
    => (forall x' y'. b x' y' -> Type x') -> Tele b x y -> Int -> (Int -> [Int]) -> [Int]
  goTele _ TNil w k = k w
  goTele payload (TCons b bs) w k =
    goTy w (payload b) <> check w (binderRaw b) <> goTele payload bs (max w (binderRaw b)) k

-- | The raw names of the binders of a term, in the order in which conversion
-- from Clash Core numbers them: depth-first, left to right, a binder after the
-- binders in its type, and a non-recursive let-binding after its right-hand
-- side. Leaves out the binders of the types of variable occurrences, which are
-- those of their binders.
binderRaws :: Term n -> [Int]
binderRaws = goTm
 where
  goTy :: Type x -> [Int]
  goTy = \case
    VarTy {} -> []
    ForAllTy b t -> goTy (tyBinderKind b) <> [binderRaw b] <> goTy t
    AppTy t1 t2 -> goTy t1 <> goTy t2
    AnnType _ t -> goTy t
    ConstTy {} -> []
    LitTy {} -> []

  goTm :: Term x -> [Int]
  goTm = \case
    Var {} -> []
    GlobalVar {} -> []
    Data {} -> []
    Literal {} -> []
    Prim {} -> []
    Lam b e -> goTy (idBinderType b) <> [binderRaw b] <> goTm e
    TyLam b e -> goTy (tyBinderKind b) <> [binderRaw b] <> goTm e
    App e1 e2 -> goTm e1 <> goTm e2
    TyApp e ty -> goTm e <> goTy ty
    Let (NonRec b x) e -> goTm x <> goTy (idBinderType b) <> [binderRaw b] <> goTm e
    Let (Rec bs xs) e -> goTele idBinderType bs <> concatMap goTm xs <> goTm e
    Case e ty alts -> goTm e <> goTy ty <> concatMap goAlt alts
    Cast e t1 t2 -> goTm e <> goTy t1 <> goTy t2
    Tick t e -> goTick t <> goTm e

  goAlt :: Alt x -> [Int]
  goAlt (Alt p e) = case p of
    DataPat _ tvs ids -> goTele tyBinderKind tvs <> goTele idBinderType ids <> goTm e
    LitPat _ -> goTm e
    DefaultPat -> goTm e

  goTick :: TickInfo x -> [Int]
  goTick = \case
    NameMod _ ty -> goTy ty
    Attributes ty e -> goTy ty <> goTm e
    _ -> []

  goTele :: HasNameBinder b => (forall x' y'. b x' y' -> Type x') -> Tele b x y -> [Int]
  goTele _ TNil = []
  goTele payload (TCons b bs) = goTy (payload b) <> [binderRaw b] <> goTele payload bs

-- | Rebuild a term, going under every binder with 'openIdBinder',
-- 'openTyBinder', 'openBind' and 'openAlt', also in the types in it. Gives the
-- problems with the binders that these produce: binders that are not above the
-- watermark of the scope, or that the watermark of the extended scope is
-- below. Doesn't go into the types of the binders of telescopes ('Rec' and
-- 'DataPat'), as these are in scopes that the traversal doesn't have, nor into
-- the types of variable occurrences, which are those of their binders (or of
-- free variables).
reopenTerm :: Distinct n => Scope n -> Term n -> ([String], Term n)
reopenTerm sc0 tm0 = swap (runWriter (goTm sc0 tm0))
 where
  swap (a, b) = (b, a)

  checkOpened :: Scope x -> Scope y -> [Int] -> Writer [String] ()
  checkOpened sc sc' ks = do
    let w = scopeWatermark sc
        w' = scopeWatermark sc'
    tell ["binder " <> show k <> " not above the watermark " <> show w | k <- ks, k <= w]
    tell ["binders " <> show ks <> " not increasing" | not (and (zipWith (<) ks (drop 1 ks)))]
    tell [ "watermark " <> show w' <> " of the extended scope is below binder " <> show k
         | k <- ks, k > w' ]

  goTy :: Distinct x => Scope x -> Type x -> Writer [String] (Type x)
  goTy sc = \case
    VarTy occ k n -> VarTy occ <$> goTy sc k <*> pure n
    ForAllTy b t -> openTyBinder sc b t $ \sc' b' t' -> do
      checkOpened sc sc' [binderRaw b']
      k <- goTy sc (tyBinderKind b')
      ForAllTy b' {tyBinderKind = k} <$> goTy sc' t'
    AppTy t1 t2 -> AppTy <$> goTy sc t1 <*> goTy sc t2
    AnnType a t -> AnnType a <$> goTy sc t
    ty@ConstTy {} -> pure ty
    ty@LitTy {} -> pure ty

  goTm :: Distinct x => Scope x -> Term x -> Writer [String] (Term x)
  goTm sc = \case
    tm@Var {} -> pure tm
    tm@GlobalVar {} -> pure tm
    tm@Data {} -> pure tm
    tm@Literal {} -> pure tm
    tm@Prim {} -> pure tm
    Lam b e -> openIdBinder sc b e $ \sc' b' e' -> do
      checkOpened sc sc' [binderRaw b']
      ty <- goTy sc (idBinderType b')
      Lam b' {idBinderType = ty} <$> goTm sc' e'
    TyLam b e -> openTyBinder sc b e $ \sc' b' e' -> do
      checkOpened sc sc' [binderRaw b']
      k <- goTy sc (tyBinderKind b')
      TyLam b' {tyBinderKind = k} <$> goTm sc' e'
    App e1 e2 -> App <$> goTm sc e1 <*> goTm sc e2
    TyApp e ty -> TyApp <$> goTm sc e <*> goTy sc ty
    Let bs e -> openBind sc bs e $ \sc' bs' e' -> case bs' of
      NonRec b x -> do
        checkOpened sc sc' [binderRaw b]
        ty <- goTy sc (idBinderType b)
        x' <- goTm sc x
        Let (NonRec b {idBinderType = ty} x') <$> goTm sc' e'
      Rec bndrs xs -> do
        checkOpened sc sc' (teleRaws bndrs)
        xs' <- mapM (goTm sc') xs
        Let (Rec bndrs xs') <$> goTm sc' e'
    Case e ty alts -> Case <$> goTm sc e <*> goTy sc ty <*> mapM (goAlt sc) alts
    Cast e t1 t2 -> Cast <$> goTm sc e <*> goTy sc t1 <*> goTy sc t2
    Tick t e -> Tick <$> goTick sc t <*> goTm sc e

  goAlt :: Distinct x => Scope x -> Alt x -> Writer [String] (Alt x)
  goAlt sc alt = openAlt sc alt $ \sc' p e -> do
    case p of
      DataPat _ tvs ids -> checkOpened sc sc' (teleRaws tvs <> teleRaws ids)
      _ -> pure ()
    Alt p <$> goTm sc' e

  goTick :: Distinct x => Scope x -> TickInfo x -> Writer [String] (TickInfo x)
  goTick sc = \case
    NameMod m ty -> NameMod m <$> goTy sc ty
    Attributes ty e -> Attributes <$> goTy sc ty <*> goTm sc e
    t -> pure t

  teleRaws :: HasNameBinder b => Tele b x y -> [Int]
  teleRaws TNil = []
  teleRaws (TCons b bs) = binderRaw b : teleRaws bs

-- * Scrambling binders

-- | Rename the binders of a term, keeping its meaning: a binder gets a random
-- name, often that of a variable in scope (given the watermark of its scope)
-- or of another binder, of either sort, as long as no occurrence in its scope
-- refers to another variable with that name. Binders of the same telescope
-- keep different names.
--
-- Values like the result arise from sinking: e.g. in @'Lam' b ('sink' v)@,
-- where @b@ is a fresh binder, binders in @v@ may have the name of @b@. The
-- trusted core has to handle them, but conversion never produces them.
scrambleBinders :: Int -> Term n -> Gen (Term n)
scrambleBinders w tm0 = evalStateT (goTm IntMap.empty tm0) 1000000
 where
  rn :: IntMap Int -> Int -> Int
  rn ren k = IntMap.findWithDefault k k ren

  -- A new name for a binder: not one of the forbidden ones
  pick :: IntSet -> Int -> Scramble Int
  pick forbidden old = do
    k <- lift (frequency [(3, choose (1, w + 6)), (1, pure old)])
    if IntSet.member k forbidden
      then if IntSet.member old forbidden then state (\f -> (f, f + 1)) else pure old
      else pure k

  -- The names of a set of names in the scope above a binder, renamed
  outer :: IntMap Int -> [Int] -> IntSet
  outer ren = IntSet.fromList . map (rn ren)

  frees :: Term x -> [Int]
  frees e = nameSetRaws (freeTmVars e) <> nameSetRaws (freeTyVars e)

  freesTy :: Type x -> [Int]
  freesTy = nameSetRaws . freeTyVarsOfType

  tyName :: IntMap Int -> TyName x -> TyName x
  tyName ren n = UnsafeTyName (UnsafeName (rn ren (tyNameRaw n)))

  tmName :: IntMap Int -> TmName x -> TmName x
  tmName ren n = UnsafeTmName (UnsafeName (rn ren (tmNameRaw n)))

  tyBinder :: Int -> TyBinder x y -> Type x -> TyBinder x y
  tyBinder k b kind = b {tyBinderKind = kind, tyBinderName = UnsafeTyNameBinder (UnsafeNameBinder (UnsafeName k))}

  idBinder :: Int -> IdBinder x y -> Type x -> IdBinder x y
  idBinder k b ty = b {idBinderType = ty, idBinderName = UnsafeTmNameBinder (UnsafeNameBinder (UnsafeName k))}

  goTy :: IntMap Int -> Type x -> Scramble (Type x)
  goTy ren = \case
    VarTy occ k n -> VarTy occ <$> goTy ren k <*> pure (tyName ren n)
    ForAllTy b t -> do
      kind <- goTy ren (tyBinderKind b)
      let o = binderRaw b
      k <- pick (outer ren (filter (/= o) (freesTy t))) o
      ForAllTy (tyBinder k b kind) <$> goTy (IntMap.insert o k ren) t
    AppTy t1 t2 -> AppTy <$> goTy ren t1 <*> goTy ren t2
    AnnType a t -> AnnType a <$> goTy ren t
    ty@ConstTy {} -> pure ty
    ty@LitTy {} -> pure ty

  goTm :: IntMap Int -> Term x -> Scramble (Term x)
  goTm ren = \case
    Var occ ty n -> Var occ <$> goTy ren ty <*> pure (tmName ren n)
    tm@GlobalVar {} -> pure tm
    tm@Data {} -> pure tm
    tm@Literal {} -> pure tm
    tm@Prim {} -> pure tm
    Lam b e -> do
      ty <- goTy ren (idBinderType b)
      let o = binderRaw b
      k <- pick (outer ren (filter (/= o) (frees e))) o
      Lam (idBinder k b ty) <$> goTm (IntMap.insert o k ren) e
    TyLam b e -> do
      kind <- goTy ren (tyBinderKind b)
      let o = binderRaw b
      k <- pick (outer ren (filter (/= o) (frees e))) o
      TyLam (tyBinder k b kind) <$> goTm (IntMap.insert o k ren) e
    App e1 e2 -> App <$> goTm ren e1 <*> goTm ren e2
    TyApp e ty -> TyApp <$> goTm ren e <*> goTy ren ty
    Let (NonRec b x) e -> do
      ty <- goTy ren (idBinderType b)
      x' <- goTm ren x
      let o = binderRaw b
      k <- pick (outer ren (filter (/= o) (frees e))) o
      Let (NonRec (idBinder k b ty) x') <$> goTm (IntMap.insert o k ren) e
    tm@(Let (Rec bs xs) e) -> do
      let forbidden = outer ren (frees tm)
      goIdTele forbidden ren bs $ \ren' bs' ->
        Let <$> (Rec bs' <$> mapM (goTm ren') xs) <*> goTm ren' e
    Case e ty alts -> Case <$> goTm ren e <*> goTy ren ty <*> mapM (goAlt ren ty) alts
    Cast e t1 t2 -> Cast <$> goTm ren e <*> goTy ren t1 <*> goTy ren t2
    Tick t e -> Tick <$> goTick ren t <*> goTm ren e

  goAlt :: IntMap Int -> Type x -> Alt x -> Scramble (Alt x)
  goAlt ren ty alt@(Alt p e) = case p of
    DataPat dc tvs ids -> do
      -- The free variables of the alternative, and the case type: more than
      -- needed
      let forbidden = outer ren (frees (Case (Literal (C.IntLiteral 0)) ty [alt]))
      goTyTele forbidden ren tvs $ \ren1 tvs' ->
        -- The fields must not capture the existentials in their types
        goIdTele (forbidden <> teleNew tvs') ren1 ids $ \ren2 ids' ->
          Alt (DataPat dc tvs' ids') <$> goTm ren2 e
    LitPat _ -> Alt p <$> goTm ren e
    DefaultPat -> Alt p <$> goTm ren e

  -- The new names of the binders of a telescope
  teleNew :: HasNameBinder b => Tele b x y -> IntSet
  teleNew TNil = IntSet.empty
  teleNew (TCons b bs) = IntSet.insert (binderRaw b) (teleNew bs)

  goTyTele
    :: IntSet -> IntMap Int -> Tele TyBinder x y
    -> (IntMap Int -> Tele TyBinder x y -> Scramble r) -> Scramble r
  goTyTele _ ren TNil k = k ren TNil
  goTyTele forbidden ren (TCons b bs) k = do
    kind <- goTy ren (tyBinderKind b)
    let o = binderRaw b
    new <- pick forbidden o
    goTyTele (IntSet.insert new forbidden) (IntMap.insert o new ren) bs $ \ren' bs' ->
      k ren' (TCons (tyBinder new b kind) bs')

  goIdTele
    :: IntSet -> IntMap Int -> Tele IdBinder x y
    -> (IntMap Int -> Tele IdBinder x y -> Scramble r) -> Scramble r
  goIdTele _ ren TNil k = k ren TNil
  goIdTele forbidden ren (TCons b bs) k = do
    ty <- goTy ren (idBinderType b)
    let o = binderRaw b
    new <- pick forbidden o
    goIdTele (IntSet.insert new forbidden) (IntMap.insert o new ren) bs $ \ren' bs' ->
      k ren' (TCons (idBinder new b ty) bs')

  goTick :: IntMap Int -> TickInfo x -> Scramble (TickInfo x)
  goTick ren = \case
    NameMod m ty -> NameMod m <$> goTy ren ty
    Attributes ty e -> Attributes <$> goTy ren ty <*> goTm ren e
    t -> pure t

-- | A property of the values that 'scrambleBinders' makes of a term in a
-- scope
forAllScrambled :: Testable prop => Scope n -> Term n -> (Term n -> prop) -> Property
forAllScrambled sc tm =
  forAllShow (scrambleBinders (scopeWatermark sc) tm) (\t -> "scrambled:\n" <> showTerm (toClashTerm t))

-- | 'scrambleBinders' with a supply of fresh names
type Scramble = StateT Int Gen

-- * Comparing Clash Core

-- | Drop the type argument of coercions: scoped Core applies @_CO_@ to its
-- type, where Clash Core keeps it in the 'C.PrimInfo', see
-- 'Test.Clash.Differential.coercionForm'. 'aeqTermTyped' doesn't compare the
-- types of primitives, so it compares coercions after this.
stripCoercionTypes :: C.Term -> C.Term
stripCoercionTypes = \case
  C.TyApp e@(C.Prim p) _ | C.primName p == "_CO_" -> e
  C.Lam v e -> C.Lam v (stripCoercionTypes e)
  C.TyLam v e -> C.TyLam v (stripCoercionTypes e)
  C.App e1 e2 -> C.App (stripCoercionTypes e1) (stripCoercionTypes e2)
  C.TyApp e ty -> C.TyApp (stripCoercionTypes e) ty
  C.Let bind e -> C.Let (stripCoercionTypes <$> bind) (stripCoercionTypes e)
  C.Case e ty alts -> C.Case (stripCoercionTypes e) ty [(p, stripCoercionTypes a) | (p, a) <- alts]
  C.Cast e t1 t2 -> C.Cast (stripCoercionTypes e) t1 t2
  C.Tick t e -> C.Tick t (stripCoercionTypes e)
  e -> e

-- | Levels of the binders on both sides, see 'aeqTermTyped'
data Levels = Levels !Int !(Map Unique Int) !(Map Unique Int) !(Map Unique Int) !(Map Unique Int)

-- | Alpha-equivalence of Clash terms that, unlike 'Clash.Core.Subst.aeqTerm',
-- also compares the types of variable occurrences, the types of the binders of
-- non-recursive let-bindings and of patterns, and the result types of
-- case-expressions, and the kinds of type variable occurrences.
aeqTermTyped :: C.Term -> C.Term -> Bool
aeqTermTyped = goTm (Levels 0 Map.empty Map.empty Map.empty Map.empty)
 where
  occ :: Map Unique Int -> Map Unique Int -> C.Var a -> C.Var a -> Bool
  occ envL envR v1 v2 = case (Map.lookup (C.varUniq v1) envL, Map.lookup (C.varUniq v2) envR) of
    (Just l1, Just l2) -> l1 == l2
    (Nothing, Nothing) -> C.varUniq v1 == C.varUniq v2
    _ -> False

  underTy, underTm :: C.Var a -> C.Var b -> Levels -> Levels
  underTy v1 v2 (Levels l tmL tmR tyL tyR) =
    Levels (l + 1) tmL tmR (Map.insert (C.varUniq v1) l tyL) (Map.insert (C.varUniq v2) l tyR)
  underTm v1 v2 (Levels l tmL tmR tyL tyR) =
    Levels (l + 1) (Map.insert (C.varUniq v1) l tmL) (Map.insert (C.varUniq v2) l tmR) tyL tyR

  goTy :: Levels -> C.Type -> C.Type -> Bool
  goTy lv@(Levels _ _ _ tyL tyR) t1 t2 = case (t1, t2) of
    (C.VarTy a1, C.VarTy a2) -> occ tyL tyR a1 a2 && goTy lv (C.varType a1) (C.varType a2)
    (C.ConstTy c1, C.ConstTy c2) -> c1 == c2
    (C.ForAllTy a1 s1, C.ForAllTy a2 s2) ->
      goTy lv (C.varType a1) (C.varType a2) && goTy (underTy a1 a2 lv) s1 s2
    (C.AppTy f1 x1, C.AppTy f2 x2) -> goTy lv f1 f2 && goTy lv x1 x2
    (C.LitTy l1, C.LitTy l2) -> l1 == l2
    (C.AnnType a1 s1, C.AnnType a2 s2) -> a1 == a2 && goTy lv s1 s2
    _ -> False

  goVarTy :: Levels -> C.Var a -> C.Var b -> Bool
  goVarTy lv v1 v2 = goTy lv (C.varType v1) (C.varType v2)

  goTm :: Levels -> C.Term -> C.Term -> Bool
  goTm lv@(Levels _ tmL tmR _ _) e1 e2 = case (e1, e2) of
    (C.Var v1, C.Var v2)
      | C.isGlobalId v1 || C.isGlobalId v2 -> C.varKey v1 == C.varKey v2
      | otherwise -> occ tmL tmR v1 v2 && goVarTy lv v1 v2
    (C.Data dc1, C.Data dc2) -> dc1 == dc2
    (C.Literal l1, C.Literal l2) -> l1 == l2
    (C.Prim p1, C.Prim p2) -> C.primName p1 == C.primName p2
    (C.Lam v1 b1, C.Lam v2 b2) -> goVarTy lv v1 v2 && goTm (underTm v1 v2 lv) b1 b2
    (C.TyLam v1 b1, C.TyLam v2 b2) -> goVarTy lv v1 v2 && goTm (underTy v1 v2 lv) b1 b2
    (C.App f1 x1, C.App f2 x2) -> goTm lv f1 f2 && goTm lv x1 x2
    (C.TyApp f1 t1, C.TyApp f2 t2) -> goTm lv f1 f2 && goTy lv t1 t2
    (C.Let (C.NonRec v1 x1) b1, C.Let (C.NonRec v2 x2) b2) ->
      goVarTy lv v1 v2 && goTm lv x1 x2 && goTm (underTm v1 v2 lv) b1 b2
    (C.Let (C.Rec bs1) b1, C.Let (C.Rec bs2) b2) ->
      length bs1 == length bs2 &&
        and (zipWith (goVarTy lv) (map fst bs1) (map fst bs2)) &&
        let lv' = List.foldl' (\acc (v1, v2) -> underTm v1 v2 acc) lv (zip (map fst bs1) (map fst bs2))
        in  and (zipWith (goTm lv') (b1 : map snd bs1) (b2 : map snd bs2))
    (C.Case s1 t1 as1, C.Case s2 t2 as2) ->
      length as1 == length as2 && goTm lv s1 s2 && goTy lv t1 t2 &&
        and (zipWith (goAlt lv) as1 as2)
    (C.Cast x1 f1 t1, C.Cast x2 f2 t2) -> goTm lv x1 x2 && goTy lv f1 f2 && goTy lv t1 t2
    (C.Tick t1 x1, C.Tick t2 x2) -> goTick lv t1 t2 && goTm lv x1 x2
    _ -> False

  goAlt :: Levels -> C.Alt -> C.Alt -> Bool
  goAlt lv (p1, e1) (p2, e2) = case (p1, p2) of
    (C.DataPat dc1 tvs1 ids1, C.DataPat dc2 tvs2 ids2) ->
      dc1 == dc2 && length tvs1 == length tvs2 && length ids1 == length ids2 &&
        goTele underTy lv (zip tvs1 tvs2) (\lv1 ->
          goTele underTm lv1 (zip ids1 ids2) (\lv2 -> goTm lv2 e1 e2))
    (C.LitPat l1, C.LitPat l2) -> l1 == l2 && goTm lv e1 e2
    (C.DefaultPat, C.DefaultPat) -> goTm lv e1 e2
    _ -> False

  goTele :: (C.Var a -> C.Var a -> Levels -> Levels) -> Levels -> [(C.Var a, C.Var a)] -> (Levels -> Bool) -> Bool
  goTele _ lv [] k = k lv
  goTele under lv ((v1, v2) : vs) k = goVarTy lv v1 v2 && goTele under (under v1 v2 lv) vs k

  goTick :: Levels -> C.TickInfo -> C.TickInfo -> Bool
  goTick lv t1 t2 = case (t1, t2) of
    (C.SrcSpan s1, C.SrcSpan s2) -> s1 == s2
    (C.NameMod m1 ty1, C.NameMod m2 ty2) -> m1 == m2 && goTy lv ty1 ty2
    (C.DeDup, C.DeDup) -> True
    (C.NoDeDup, C.NoDeDup) -> True
    (C.Attributes ty1 x1, C.Attributes ty2 x2) -> goTy lv ty1 ty2 && goTm lv x1 x2
    _ -> False

-- * Properties

-- | Run the QuickCheck properties of a test tree with the given number of
-- tests, unless the number of tests is given on the command line: the
-- unittests run 10000 tests of every property by default (see
-- @unittests.hs@), which is too slow for properties on generated terms. So
-- @--quickcheck-tests 10000@ runs the given number of tests too.
withTests :: Int -> TestTree -> TestTree
withTests n = adjustOption $ \(QuickCheckTests k) ->
  QuickCheckTests (if k == 10000 then n else k)

-- | Run a scoped transformation on a closed term, in the root context, in the
-- default (silent) environment. Gives the result, the number of
-- transformations applied, and whether a change was signaled.
runScopedClosed :: ScopedRewrite () -> Term 'VoidS -> IO (Term 'VoidS, Word, Bool)
runScopedClosed t e = do
  (e', st, changed) <- runRewriteTest def def (runScopedTransform t (rootContext emptyScope) e)
  pure (e', _transformCounter st, changed)

-- | Run a scoped transformation on a Clash term converted to scoped Core, in
-- the root context of the scope of its free variables. Gives the result,
-- converted back (its free variables keep their uniques), the final state,
-- and whether a change was signaled.
runScopedOpen
  :: RewriteEnv
  -> RewriteState extra
  -> ScopedRewrite extra
  -> C.Term
  -> IO (C.Term, RewriteState extra, Bool)
runScopedOpen env st t e =
  runScopedOn env st e (\sc e' -> toClashTerm <$> runScopedTransform t (rootContext sc) e')

-- | Run a scoped transformation on a Clash term through
-- 'Clash.Rewrite.Scoped.ViaFoil.viaFoil', in the given context. Gives the
-- result, the final state, and whether a change was signaled.
runViaFoil
  :: RewriteEnv
  -> RewriteState extra
  -> TransformContext
  -> ScopedRewrite extra
  -> C.Term
  -> IO (C.Term, RewriteState extra, Bool)
runViaFoil env st ctx t e = runRewriteTest env st (viaFoil "test" t ctx e)

-- | A primitive @somePrim :: T@, with no special meaning
somePrim :: C.PrimInfo
somePrim = C.PrimInfo
  { C.primName = "somePrim"
  , C.primType = C.ConstTy (C.TyCon (C.mkUnsafeSystemName "T" 0))
  , C.primWorkInfo = C.WorkConstant
  , C.primMultiResult = C.SingleResult
  , C.primUnfolding = C.NoUnfolding
  }

-- | Run an action on a Clash term converted to scoped Core, in the scope of
-- its free variables. Gives the result, the final state, and whether a change
-- was signaled.
runScopedOn
  :: RewriteEnv
  -> RewriteState extra
  -> C.Term
  -> (forall n. Distinct n => Scope n -> Term n -> RewriteMonad extra a)
  -> IO (a, RewriteState extra, Bool)
runScopedOn env st tm k =
  runRewriteTest env st (withFreeVarsOf [tm] (\cenv -> k (convScope cenv) (fromClashTerm cenv tm)))

-- | Assert that a scoped 'Clash.Rewrite.StrategyDSL.onLet' handler agrees
-- with one of Clash Core on a let-expression, run in the root context of the
-- scope of its free variables: the results are alpha-equivalent (comparing
-- types too, see 'aeqTermTyped'), and both signal a change or neither does.
assertLetHandlerAgrees
  :: HasCallStack
  => RewriteEnv
  -> RewriteState extra
  -> (C.TransformContext -> C.Term -> C.Bind C.Term -> C.Term -> RewriteMonad extra C.Term)
  -- ^ The handler of Clash Core
  -> (forall n l. Distinct n => Scoped.TransformContext n -> Term n -> Bind n l -> Term l -> RewriteMonad extra (Term n))
  -- ^ The scoped handler
  -> C.Term
  -- ^ A let-expression
  -> Assertion
assertLetHandlerAgrees env st old new tm = case tm of
  C.Let bind body -> do
    let is = inScopeOf (Lens.toListOf C.freeLocalIds tm) `unionInScope` inScopeOf (Lens.toListOf C.termFreeTyVars tm)
    (expected, st1, changed1) <-
      runRewriteTest env st (old (C.TransformContext is []) tm bind body)
    (actual, st2, changed2) <- runScopedOn env st tm $ \sc -> \case
      e@(Let bind' body') -> toClashTerm <$> new (rootContext sc) e bind' body'
      _ -> error "assertLetHandlerAgrees: not a let-expression"
    assertBool
      ("Clash Core:\n" <> showTerm expected <> "\nscoped:\n" <> showTerm actual)
      (aeqTermTyped (globalsByName expected) (globalsByName actual))
    assertBool ("change signaled: Clash Core " <> show changed1 <> ", scoped " <> show changed2)
      (changed1 == changed2)
    assertNewBindingsAgree st st1 st2
  _ -> assertFailure "assertLetHandlerAgrees: not a let-expression"

-- | Assert that a rewrite of Clash Core and its port agree on a term, in the
-- given context: the results are alpha-equivalent (comparing types too, see
-- 'aeqTermTyped'), both signal a change or neither does, and they add the same
-- global bindings (see 'assertNewBindingsAgree'). Give the port as the
-- normalizer runs it: e.g. a scoped transformation compiled with
-- 'Clash.Rewrite.StrategyDSL.TH.asRewriteQ', which runs it through
-- 'Clash.Rewrite.Scoped.ViaFoil.viaFoil'.
assertRewritesAgree
  :: HasCallStack
  => RewriteEnv
  -> RewriteState extra
  -> TransformContext
  -> Rewrite extra
  -- ^ The rewrite of Clash Core
  -> Rewrite extra
  -- ^ Its port
  -> C.Term
  -> Assertion
assertRewritesAgree env st ctx old new tm = do
  (expected, st1, changed1) <- runRewriteTest env st (old ctx tm)
  (actual, st2, changed2) <- runRewriteTest env st (new ctx tm)
  assertBool
    ("Clash Core:\n" <> showTerm expected <> "\nport:\n" <> showTerm actual)
    (aeqTermTyped (globalsByName expected) (globalsByName actual))
  assertBool ("change signaled: Clash Core " <> show changed1 <> ", port " <> show changed2)
    (changed1 == changed2)
  assertNewBindingsAgree st st1 st2

-- | Like 'assertRewritesAgree', but the results only need to be equal up to
-- the given known differences (see
-- 'Test.Clash.Differential.Sites.compareAtSite'), and the global bindings they
-- add are not compared. Gives the outcome, so a test can check which known
-- differences it needed.
assertRewritesAgreeUpTo
  :: HasCallStack
  => [KnownDifference]
  -> RewriteEnv
  -> RewriteState extra
  -> TransformContext
  -> Rewrite extra
  -- ^ The rewrite of Clash Core
  -> Rewrite extra
  -- ^ Its port
  -> C.Term
  -> IO Outcome
assertRewritesAgreeUpTo kds env st ctx old new tm = do
  (expected, _, changed1) <- runRewriteTest env st (old ctx tm)
  (actual, _, changed2) <- runRewriteTest env st (new ctx tm)
  let outcome = compareAtSite kds expected actual
  case outcome of
    Different _ ->
      assertFailure ("Clash Core:\n" <> showTerm expected <> "\nport:\n" <> showTerm actual)
    _ -> pure ()
  assertBool ("change signaled: Clash Core " <> show changed1 <> ", port " <> show changed2)
    (changed1 == changed2)
  pure outcome

-- | Assert that two runs from the first state added global bindings with the
-- same names, whose bodies are alpha-equivalent, up to the uniques of the
-- globals they refer to (see 'globalsByName')
assertNewBindingsAgree
  :: HasCallStack => RewriteState extra -> RewriteState extra -> RewriteState extra -> Assertion
assertNewBindingsAgree st0 st1 st2 = do
  let new st = Map.fromList
        [ (C.nameOcc (C.varName (bindingId b)), bindingTerm b)
        | b <- UniqMap.elems (_bindings st)
        , not (UniqMap.elem (C.varUniq (bindingId b)) (_bindings st0)) ]
      new1 = new st1
      new2 = new st2
  assertBool ("new global bindings: Clash Core " <> show (Map.keys new1) <> ", scoped " <> show (Map.keys new2))
    (Map.keys new1 == Map.keys new2)
  sequence_
    [ assertBool
        ("global binding " <> show nm <> ", Clash Core:\n" <> showTerm b1 <> "\nscoped:\n" <> showTerm b2)
        (aeqTermTyped (globalsByName b1) (globalsByName b2))
    | (nm, (b1, b2)) <- Map.toList (Map.intersectionWith (,) new1 new2) ]

-- | A step down a term, to one of its subterms, see 'atPath'
data Step
  = LamStep
  -- ^ The body of a lambda
  | TyLamStep
  -- ^ The body of a type lambda
  | LetBodyStep
  -- ^ The body of a let-expression
  | LetRhsStep Int
  -- ^ The right-hand side with the given index of a let-expression
  | AltStep Int
  -- ^ The right-hand side of the alternative with the given index
  | AppFunStep
  -- ^ The function of an application
  | AppArgStep
  -- ^ The argument of an application
  deriving (Show)

-- | Go down a term along a path, opening the binders on the way, and continue
-- with the context of the subterm, and the subterm. Errors if the path doesn't
-- fit the term.
atPath
  :: HasCallStack
  => Distinct n
  => [Step]
  -> Scoped.TransformContext n
  -> Term n
  -> (forall l. Distinct l => Scoped.TransformContext l -> Term l -> r)
  -> r
atPath [] ctx e k = k ctx e
atPath (st : sts) ctx e k = case (st, e) of
  (LamStep, Lam b body) -> underLam ctx b body $ \ctx' _ body' -> atPath sts ctx' body' k
  (TyLamStep, TyLam b body) -> underTyLam ctx b body $ \ctx' _ body' -> atPath sts ctx' body' k
  (LetBodyStep, Let bind body) -> underLet ctx bind body $ \ctx' _ body' -> atPath sts ctx' body' k
  (LetRhsStep 0, Let (NonRec b x) _) -> atPath sts (pushFrame (LetBinding b) ctx) x k
  (LetRhsStep i, Let (Rec bs xs) body) | i < length xs ->
    underLetRec ctx bs xs body $ \_ ctxRhs _ xs' _ -> atPath sts (ctxRhs i) (xs' !! i) k
  (AltStep i, Case _ _ alts) | i < length alts ->
    underAlt ctx (alts !! i) $ \ctx' _ e' -> atPath sts ctx' e' k
  (AppFunStep, App f _) -> atPath sts (pushFrame Scoped.AppFun ctx) f k
  (AppArgStep, App f a) -> atPath sts (pushFrame (Scoped.appArgFrame f) ctx) a k
  _ -> error ("atPath: " <> show st <> " doesn't fit the term:\n" <> showTerm (toClashTerm e))
