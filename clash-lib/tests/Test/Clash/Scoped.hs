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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Clash.Scoped
  ( -- * Converting
    withFreeVarsOf
  , withFreeVars
  , extendFreeVars
  , sinkInto
  , roundTrip
  , extendSunk
  , sunkVar
  , withFreeTyVarsOf
  , withTypeScopes
    -- * Assertions
  , assertScopedAlphaEq
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
    -- * Properties
  , withTests
    -- * Type constructors
  , fixtureTyConMap
  , tcTy
  , boolTy
  , maybeTy
  , nothingDataCon
  , justDataCon
  , nTy
  , pTy
  , wTy
  , fTy
  , maxTy
  , kTy
  , qTy
  , signalTy
  , addTy
  , cmpNatTy
  , unconsSymbolTy
  , eqTy
  , vTy
  , vNilDataCon
  , vConsDataCon
  ) where

import qualified Control.Lens as Lens
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, state)
import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack)
import Test.Tasty (TestTree, adjustOption)
import Test.Tasty.HUnit (Assertion, assertFailure)
import Test.Tasty.QuickCheck
  (Gen, Property, QuickCheckTests (..), Testable, choose, forAllShow, frequency)

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.FreeVars as C
import qualified Clash.Core.Literal as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Var as C
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Unique (Unique, fromGhcUnique)
import GHC.Builtin.Names
  ( ordEQDataConKey, ordGTDataConKey, ordLTDataConKey, typeNatAddTyFamNameKey
  , typeNatCmpTyFamNameKey, typeUnconsSymbolTyFamNameKey )

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

import Test.Clash.Rewrite
  ( assertAlphaEq, intId, intTy, intTyCon, mkAlgTyCon, mkDataCon, mkFunTyCon
  , mkNewTyCon, mkTyConName, tyVar )
import Test.Clash.Scoped.Gen (FreeVars (..), showTerm)

-- | A conversion environment with the free variables of the given Clash terms
-- (both type and term variables), which keep their uniques
withFreeVarsOf :: [C.Term] -> (forall n. Distinct n => ConvEnv n -> r) -> r
withFreeVarsOf tms = withConvEnv tvs ids
 where
  tvs = List.nubBy sameUniq (concatMap (Lens.toListOf C.termFreeTyVars) tms)
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

-- | Convert a Clash term to scoped Core and back, in the scope of its free
-- variables
roundTrip :: HasCallStack => C.Term -> C.Term
roundTrip tm = withFreeVarsOf [tm] $ \env -> toClashTerm (fromClashTerm env tm)

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

-- * Type constructors
--
-- A type constructor environment ('fixtureTyConMap') with data types,
-- newtypes and type families, to test functions that look through them.

-- | Type variables of the type constructors
fxA, fxB :: C.TyVar
fxA = tyVar C.User "a" 3001
fxB = tyVar C.User "b" 3002

-- | The type of a type constructor
tcTy :: C.TyConName -> C.Type
tcTy = C.ConstTy . C.TyCon

-- | A type constructor applied to types
tcApp :: C.TyConName -> [C.Type] -> C.Type
tcApp = C.mkTyConApp

boolTyConName, maybeTyConName, nTyConName, pTyConName, wTyConName, fTyConName,
  maxTyConName, kTyConName, qTyConName, signalTyConName, t2TyConName,
  vTyConName :: C.TyConName
boolTyConName = mkTyConName "Bool" 3010
maybeTyConName = mkTyConName "Maybe" 3011
nTyConName = mkTyConName "N" 3012
pTyConName = mkTyConName "P" 3013
wTyConName = mkTyConName "W" 3014
fTyConName = mkTyConName "F" 3015
maxTyConName = mkTyConName "Max" 3016
kTyConName = mkTyConName "K" 3017
qTyConName = mkTyConName "Q" 3018
signalTyConName = mkTyConName "Clash.Signal.Internal.Signal" 3019
t2TyConName = mkTyConName "T2" 3024
vTyConName = mkTyConName "V" 3025

-- | @data Bool = False | True@
boolTy :: C.Type
boolTy = tcTy boolTyConName

-- | @data Maybe a = Nothing | Just a@
maybeTy :: C.Type -> C.Type
maybeTy a = tcApp maybeTyConName [a]

nothingDataCon, justDataCon :: C.DataCon
nothingDataCon = mkDataCon "Nothing" 3020 [fxA] [] [] (maybeTy (C.VarTy fxA))
justDataCon =
  (mkDataCon "Just" 3021 [fxA] [] [C.VarTy fxA] (maybeTy (C.VarTy fxA))) {C.dcTag = 2}

-- | @newtype N a = MkN (Maybe a)@
nTy :: C.Type -> C.Type
nTy a = tcApp nTyConName [a]

-- | @newtype P a = MkP (forall b. a -> b)@, whose representation type binds a
-- type variable
pTy :: C.Type -> C.Type
pTy a = tcApp pTyConName [a]

-- | @newtype W = MkW (Int -> Int)@
wTy :: C.Type
wTy = tcTy wTyConName

-- | @type family F a where F Int = Bool; F (Maybe a) = a; F a = N a@
fTy :: C.Type -> C.Type
fTy a = tcApp fTyConName [a]

-- | @type family Max a b where Max a a = a; Max a b = Maybe b@
maxTy :: C.Type -> C.Type -> C.Type
maxTy a b = tcApp maxTyConName [a, b]

-- | @type family K :: Type -> Type where K = Maybe@, which returns a type
-- constructor that is applied to the remaining arguments
kTy :: C.Type
kTy = tcTy kTyConName

-- | @type family Q a where Q a = forall b. a -> b@, whose right-hand side
-- binds a type variable
qTy :: C.Type -> C.Type
qTy a = tcApp qTyConName [a]

-- | @Signal dom a@
signalTy :: C.Type -> C.Type -> C.Type
signalTy dom a = tcApp signalTyConName [dom, a]

-- | GHC's @+@ on type-level naturals
addTy :: C.Type -> C.Type -> C.Type
addTy a b = tcApp addTyConName [a, b]

addTyConName :: C.TyConName
addTyConName =
  C.Name C.User "GHC.TypeNats.+" (fromGhcUnique typeNatAddTyFamNameKey) C.noSrcSpan

-- | GHC's @CmpNat@
cmpNatTy :: C.Type -> C.Type -> C.Type
cmpNatTy a b = tcApp cmpNatTyConName [a, b]

cmpNatTyConName :: C.TyConName
cmpNatTyConName =
  C.Name C.User "GHC.TypeNats.CmpNat" (fromGhcUnique typeNatCmpTyFamNameKey) C.noSrcSpan

-- | GHC's @UnconsSymbol :: Symbol -> Maybe (T2 Char Symbol)@, with tuple type
-- @T2@
unconsSymbolTy :: C.Type -> C.Type
unconsSymbolTy s = tcApp unconsSymbolTyConName [s]

unconsSymbolTyConName :: C.TyConName
unconsSymbolTyConName =
  C.Name C.User "GHC.TypeLits.UnconsSymbol" (fromGhcUnique typeUnconsSymbolTyFamNameKey) C.noSrcSpan

-- | Unlifted equality, @a ~# b@, of types of kind @k@
eqTy :: C.Kind -> C.Type -> C.Type -> C.Type
eqTy k a b = foldl C.AppTy C.eqPrimTy [k, k, a, b]

-- | @data V n where VNil :: (n ~ 0) => V n; VCons :: forall m. (n ~ m + 1) =>
-- Int -> V m -> V n@, a data type with equality constraints and an
-- existentially quantified type variable, like @Vec@
vTy :: C.Type -> C.Type
vTy n = tcApp vTyConName [n]

fxN, fxM :: C.TyVar
fxN = C.TyVar (C.mkUnsafeName C.User "n" 3026) 3026 C.typeNatKind
fxM = C.TyVar (C.mkUnsafeName C.User "m" 3027) 3027 C.typeNatKind

vNilDataCon, vConsDataCon :: C.DataCon
vNilDataCon =
  mkDataCon "VNil" 3028 [fxN] [] [eqTy C.typeNatKind (C.VarTy fxN) (C.LitTy (C.NumTy 0))]
    (vTy (C.VarTy fxN))
vConsDataCon =
  (mkDataCon "VCons" 3029 [fxN] [fxM]
    [ eqTy C.typeNatKind (C.VarTy fxN) (addTy (C.VarTy fxM) (C.LitTy (C.NumTy 1)))
    , intTy
    , vTy (C.VarTy fxM) ]
    (vTy (C.VarTy fxN))) {C.dcTag = 2}

-- | The type constructors of the types above, and those of
-- 'C.tysPrimMap' and 'intTyCon'
fixtureTyConMap :: C.TyConMap
fixtureTyConMap = UniqMap.fromList [(C.tyConName tc, tc) | tc <- tcs]
 where
  tcs = UniqMap.elems C.tysPrimMap <>
    [ intTyCon
    , mkAlgTyCon boolTyConName [] [falseDc, trueDc]
    , mkAlgTyCon maybeTyConName [fxA] [nothingDataCon, justDataCon]
    , mkNewTyCon nTyConName [fxA]
        (mkDataCon "MkN" 3030 [fxA] [] [maybeTy (C.VarTy fxA)] (nTy (C.VarTy fxA)))
        ([fxA], maybeTy (C.VarTy fxA))
    , mkNewTyCon pTyConName [fxA]
        (mkDataCon "MkP" 3031 [fxA] [] [pRhs] (pTy (C.VarTy fxA)))
        ([fxA], pRhs)
    , mkNewTyCon wTyConName []
        (mkDataCon "MkW" 3032 [] [] [C.mkFunTy intTy intTy] wTy)
        ([], C.mkFunTy intTy intTy)
    , mkFunTyCon fTyConName (C.mkFunTy C.liftedTypeKind C.liftedTypeKind) 1
        [ ([intTy], boolTy)
        , ([maybeTy (C.VarTy fxA)], C.VarTy fxA)
        , ([C.VarTy fxA], nTy (C.VarTy fxA)) ]
    , mkFunTyCon maxTyConName (C.mkFunTy C.liftedTypeKind (C.mkFunTy C.liftedTypeKind C.liftedTypeKind)) 2
        [ ([C.VarTy fxA, C.VarTy fxA], C.VarTy fxA)
        , ([C.VarTy fxA, C.VarTy fxB], maybeTy (C.VarTy fxB)) ]
    , mkFunTyCon kTyConName (C.mkFunTy C.liftedTypeKind C.liftedTypeKind) 0
        [ ([], tcTy maybeTyConName) ]
    , mkFunTyCon qTyConName (C.mkFunTy C.liftedTypeKind C.liftedTypeKind) 1
        [ ([C.VarTy fxA], pRhs) ]
    , mkAlgTyCon signalTyConName [fxA, fxB] []
    , mkFunTyCon addTyConName natNatNat 2 []
    , mkFunTyCon cmpNatTyConName natNatNat 2 []
    , mkFunTyCon unconsSymbolTyConName
        (C.mkFunTy C.typeSymbolKind (maybeTy (tcApp t2TyConName [C.charPrimTy, C.typeSymbolKind])))
        1 []
    , mkAlgTyCon t2TyConName [fxA, fxB]
        [mkDataCon "MkT2" 3033 [fxA, fxB] [] [C.VarTy fxA, C.VarTy fxB]
          (tcApp t2TyConName [C.VarTy fxA, C.VarTy fxB])]
    , (mkAlgTyCon vTyConName [] [vNilDataCon, vConsDataCon])
        {C.tyConKind = C.mkFunTy C.typeNatKind C.liftedTypeKind, C.tyConArity = 1}
    ] <>
    -- Promoted data constructors that type families return. Their kinds don't
    -- matter here.
    [ C.PrimTyCon (C.nameUniq nm) nm C.liftedTypeKind 0
    | nm <-
        [ C.Name C.User "GHC.Types.LT" (fromGhcUnique ordLTDataConKey) C.noSrcSpan
        , C.Name C.User "GHC.Types.EQ" (fromGhcUnique ordEQDataConKey) C.noSrcSpan
        , C.Name C.User "GHC.Types.GT" (fromGhcUnique ordGTDataConKey) C.noSrcSpan
        , mkTyConName "Nothing" 3020
        , mkTyConName "Just" 3021
        , mkTyConName "MkT2" 3033
        ]
    ]

  natNatNat = C.mkFunTy C.typeNatKind (C.mkFunTy C.typeNatKind C.typeNatKind)

  falseDc = mkDataCon "False" 3034 [] [] [] boolTy
  trueDc = (mkDataCon "True" 3035 [] [] [] boolTy) {C.dcTag = 2}

  -- forall b. a -> b
  pRhs = C.ForAllTy fxB (C.mkFunTy (C.VarTy fxA) (C.VarTy fxB))
