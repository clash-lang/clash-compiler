{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Capture-avoiding substitution, going under binders, creating fresh binders,
  and moving values out of the scope of binders, for scoped Clash Core.

  = Substitutions

  A @'Subst' i o@ maps the variables of scope @i@ to types and terms in scope
  @o@. Variables without an entry map to themselves, which is why the only
  ways to build a substitution are 'idSubst', 'closedSubst', and extending one
  with a /binder/ ('extendTySubst', 'extendTmSubst'): you can only substitute
  for a variable whose binder you are eliminating. E.g. to beta-reduce
  @'App' ('Lam' b e) arg@ in scope @n@:

  @
  'substTm' ('extendTmSubst' ('idSubst' sc) b arg) e
  @

  The exception is a substitution from a scope to itself ('tmVarsSubst'),
  which may substitute for any of its variables, as the result stays in that
  scope: e.g. to inline the bindings of a let-expression, after opening it.

  Like Clash's 'Clash.Core.Subst.Subst', substitution renames a binder only if
  it may clash with the output scope: if it is at or below the watermark of the
  scope (see 'Scope'). When a binder's type changes or the binder is renamed,
  its occurrences are mapped to the updated variable.

  = Going under binders

  A traversal that goes under a binder needs the scope extended with it, to
  create fresh binders there. 'openIdBinder', 'openTyBinder', 'openAlt' and
  'openBind' provide it. They rename the binder (and its scope) only if it
  clashes with the scope, which is rare: conversion numbers binders such that
  names increase along every path from the root.

  = Trusted code

  This module is part of the trusted core of "Clash.Core.Scoped". Its unsafe
  operations, the invariants they rely on, and why these hold:

  * __The 'Subst' invariant__: every type variable of scope @i@ without an
    entry in the type map, and every term variable of scope @i@ without an
    entry in the term map, is a variable of scope @o@; the entries are in
    scope @o@. 'idSubst' has no entries and @i = o@. 'closedSubst' has no
    entries and @i@ has no variables. 'extendTySubst' and 'extendTmSubst' add
    an entry for the new variable. 'extendSubstScope' extends @o@.
    'tmVarsSubst' has @i = o@, and entries in @o@.
    'substTyBinder' and 'substIdBinder' (through 'substTyBinderK' and
    'substIdBinderK', which get the substituted kind or type) extend @o@ with
    a fresh binder, and add an entry for the old one unless it keeps its name
    and there are no type entries; then the variable of @i'@ is the new binder
    of @o'@. (Without type entries its kind or type is unchanged, except in
    'freshenDistinct', where it is alpha-equivalent.) When the old binder shadows a variable that has an entry,
    that entry is removed or overwritten. 'freshen' only sets a flag.
    'freshenDistinct' only raises the watermark of the output scope (with
    'raiseWatermark'), which keeps its names.

  * __Substitution__ (@coerceTy@, @coerceTm@, @coerceTyName@,
    @coerceTmName@): a variable without an entry is kept as it is, which by the
    invariant is a variable of @o@. The type of a term variable occurrence and
    the kind of a type variable occurrence are substituted too, if there are
    type entries: normally they only mention variables that are in scope at
    the binder, but a type or term built by hand may have an occurrence with
    another type. A type or term without entries for it (no type entries for a
    type, no entries at all for a term) is kept as it is, as all its free
    variables are variables of @o@.

  * __Fresh binders__ ('UnsafeTyNameBinder', 'UnsafeTmNameBinder'): only
    applied to binders that 'withFresh' or 'withRefreshed' just created, see
    "Clash.Core.Scoped.Internal.Names".

  * __Opening binders__ ('unsafeCoerce' of a binder and its scope): if the
    names a binder (or pattern, or let-binding) binds are fresh for the scope,
    as 'withFreshRaw' and 'withFreshRaws' check, then the scope @l@ that the
    binder extends @n@ to has the same names as the fresh extension @l'@ that
    they provide. So the binder and the values in its scope are valid in @l'@
    as they are.

  * __Unsinking__ ('unsafeCoerce' in 'unsinkTerm' and friends): a value in
    scope @l@ in which none of the names bound between @n@ and @l@ occur free
    only mentions names of @n@. "Clash.Core.Scoped.FreeVars" checks this,
    including the types of variable occurrences and the kinds of type variable
    occurrences.

  * __Closing__ ('unsafeCoerce' in 'closeTerm' and 'closeType'): a value
    without free local variables mentions no names at all, so it is a value in
    the empty scope 'VoidS'. 'Clash.Core.Scoped.FreeVars.hasFreeVars' and
    'Clash.Core.Scoped.FreeVars.hasFreeVarsType' check this, including the
    types of variable occurrences and the kinds of type variable occurrences.

  * __Strengthening__ ('unsafeCoerce' in 'strengthenTyId',
    'strengthenTyIds' and 'teleIdBinderTypes'): a type only mentions type
    variables. A term variable binder binds no type variable, and the type
    variables in scope @l@ are those of @n@, so a type in @l@ is a type in
    @n@. This relies on binders being sorted, see
    "Clash.Core.Scoped.Internal.Names".

  * __Variables of binders__ ('unsafeCoerce' in @unsafeTeleIdVars@ and
    @unsafeTeleTyVars@, which 'teleIdVars' and 'patVars' apply to the
    telescopes that extend a scope to @l@): every binder of a telescope or
    pattern that extends a scope to
    @l@ binds a name of @l@, and as @l@ is t'Distinct', no binder after it
    shadows that name. Its type or kind lives in the scope extended by the
    binders before it, all of whose names are names of @l@ too.

  * __Free variable occurrences__ ('unsafeCoerce' in 'freeVarOccs' and
    'freeTyVarOccsOfType'): an occurrence of a variable that no binder on the
    path to it binds is an occurrence of a variable of the scope of the term
    (or type). Its kind or type is one of that scope too if it doesn't
    mention a variable bound on the path, which is checked.

  * __Closing over variables__ ('closeOver'): the telescopes bind the given
    variables in the empty scope, with their raw names and sorts
    ('UnsafeTyNameBinder' and 'UnsafeTmNameBinder' of names that are fresh
    for the empty scope). Their names are checked to be distinct, so the
    scopes the telescopes extend the empty scope to are t'Distinct'. The kind
    of every type variable binder only mentions the type variables before it,
    and the type of every term variable binder only the type variables, which
    is checked: these are names of the scopes the kinds and types live in. A
    type or term of scope @n@ whose free type variables and free term
    variables are among the given ones ('closureType', 'closureTerm', which
    check this) only mentions names of the scope @l@ the telescopes extend the
    empty scope to, with the same raw names and sorts, so it is a value of
    @l@ as it is. The watermark of @l@ is the largest of these names, which
    keeps the 'Scope' invariant.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

module Clash.Core.Scoped.Subst
  ( -- * Substitutions
    Subst
  , substScope
  , idSubst
  , closedSubst
  , extendTySubst
  , extendTmSubst
  , extendTySubstTele
  , extendTmSubstTele
  , extendSubstScope
  , tmVarsSubst
    -- * Applying substitutions
  , HasSubst (..)
  , substTy
  , substTm
  , substAlt
  , substTickInfo
  , substTyBinder
  , substIdBinder
  , substTele
  , substPat
  , substBind
  , freshen
  , freshenDistinct
    -- * Going under binders
  , openTyBinder
  , openIdBinder
  , openAlt
  , openBind
    -- * Fresh binders
  , withFreshTyBinder
  , withFreshIdBinder
  , withFreshIdBinders
    -- * Moving out of the scope of binders
  , unsinkTerm
  , unsinkType
  , unsinkTyBinder
  , unsinkIdBinder
  , unsinkAlt
  , strengthenTyId
  , strengthenTyIds
  , strengthenTyPat
  , teleIdBinderTypes
  , closeTerm
  , closeType
    -- * Global variables
  , substGlobals
    -- * Variables of binders
  , teleIdVars
  , patVars
    -- * Closing over variables
  , freeVarOccs
  , freeTyVarOccsOfType
  , Closure (..)
  , closeOver
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Kind as K
import qualified Data.List as List
import GHC.Stack (HasCallStack)
import Unsafe.Coerce (unsafeCoerce)

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C

import Clash.Core.Scoped.FreeVars
import Clash.Core.Scoped.NameMap (GlobalEnv, lookupGlobalEnv, nullGlobalEnv)
import Clash.Core.Scoped.Internal.Names
import Clash.Core.Scoped.Internal.Scope
import Clash.Core.Scoped.Syntax

-- * Substitutions

-- | A substitution from scope @i@ to scope @o@. See the module header.
data Subst (i :: S) (o :: S) = UnsafeSubst
  { substScope :: !(Scope o)
  -- ^ The output scope
  , _substTys :: !(IntMap (Type o))
  -- ^ Types for type variables, by raw name
  , _substTms :: !(IntMap (Term o))
  -- ^ Terms for term variables, by raw name
  , _substFresh :: !Bool
  -- ^ Rename every binder, see 'freshen'
  }
type role Subst nominal nominal

-- | The identity substitution
idSubst :: Scope n -> Subst n n
idSubst sc = UnsafeSubst sc IntMap.empty IntMap.empty False

-- | The substitution from the empty scope, to move a closed value into a
-- scope, e.g. the type of a data constructor
closedSubst :: Scope o -> Subst 'VoidS o
closedSubst sc = UnsafeSubst sc IntMap.empty IntMap.empty False

-- | Substitute a type for the variable bound by a type binder
extendTySubst :: Subst i o -> TyBinder i i' -> Type o -> Subst i' o
extendTySubst (UnsafeSubst sc tys tms fr) b ty =
  UnsafeSubst sc (IntMap.insert k ty tys) (IntMap.delete k tms) fr
 where
  k = tyNameRaw (tyNameOf b)

-- | Substitute a term for the variable bound by a term binder
extendTmSubst :: Subst i o -> IdBinder i i' -> Term o -> Subst i' o
extendTmSubst (UnsafeSubst sc tys tms fr) b tm =
  UnsafeSubst sc (IntMap.delete k tys) (IntMap.insert k tm tms) fr
 where
  k = tmNameRaw (tmNameOf b)

-- | Substitute types for the variables bound by a telescope. 'Nothing' if
-- there are more or fewer types than binders.
extendTySubstTele :: Subst i o -> Tele TyBinder i i' -> [Type o] -> Maybe (Subst i' o)
extendTySubstTele s TNil [] = Just s
extendTySubstTele s (TCons b bs) (t:ts) = extendTySubstTele (extendTySubst s b t) bs ts
extendTySubstTele _ _ _ = Nothing

-- | Substitute terms for the variables bound by a telescope. 'Nothing' if
-- there are more or fewer terms than binders.
extendTmSubstTele :: Subst i o -> Tele IdBinder i i' -> [Term o] -> Maybe (Subst i' o)
extendTmSubstTele s TNil [] = Just s
extendTmSubstTele s (TCons b bs) (t:ts) = extendTmSubstTele (extendTmSubst s b t) bs ts
extendTmSubstTele _ _ _ = Nothing

-- | Substitute terms for variables of a scope, keeping the result in that
-- scope: e.g. for the variables of a let-expression that was opened
-- ('openBind'), to inline some of them.
tmVarsSubst :: Scope n -> NameMap TmName n (Term n) -> Subst n n
tmVarsSubst sc m =
  UnsafeSubst sc IntMap.empty (IntMap.fromList [(tmNameRaw x, e) | (x, e) <- nameMapToList m]) False

-- | Move the output of a substitution into an extended scope
extendSubstScope :: DExt o o' => Scope o' -> Subst i o -> Subst i o'
extendSubstScope sc (UnsafeSubst _ tys tms fr) = UnsafeSubst sc (sink1 tys) (sink1 tms) fr
{-# INLINE extendSubstScope #-}

-- Trusted: see the module header
coerceTy :: Type i -> Type o
coerceTy = unsafeCoerce
{-# INLINE coerceTy #-}

-- Trusted: see the module header
coerceTm :: Term i -> Term o
coerceTm = unsafeCoerce
{-# INLINE coerceTm #-}

-- Trusted: see the module header
coerceTyName :: TyName i -> TyName o
coerceTyName = unsafeCoerce
{-# INLINE coerceTyName #-}

-- Trusted: see the module header
coerceTmName :: TmName i -> TmName o
coerceTmName = unsafeCoerce
{-# INLINE coerceTmName #-}

-- * Applying substitutions

-- | Values that a substitution can be applied to
class HasSubst (e :: S -> K.Type) where
  -- | Apply a substitution
  subst :: Distinct o => Subst i o -> e i -> e o

instance HasSubst Type where
  subst = substTy
  {-# INLINE subst #-}

instance HasSubst Term where
  subst = substTm
  {-# INLINE subst #-}

instance HasSubst Alt where
  subst = substAlt
  {-# INLINE subst #-}

instance HasSubst TickInfo where
  subst = substTickInfo
  {-# INLINE subst #-}

-- | Apply a substitution to a type
substTy :: Distinct o => Subst i o -> Type i -> Type o
substTy s@(UnsafeSubst _ tys _ fr) ty0
  | IntMap.null tys && not fr = coerceTy ty0
  | otherwise = go ty0
 where
  go ty = case ty of
    VarTy occ k n -> case IntMap.lookup (tyNameRaw n) tys of
      Just ty' -> ty'
      -- The kind of the occurrence may mention a substituted variable, see the
      -- module header
      Nothing -> VarTy occ (go k) (coerceTyName n)
    ConstTy c -> ConstTy c
    ForAllTy b t -> substTyBinder s b $ \s' b' -> ForAllTy b' (substTy s' t)
    AppTy t1 t2 -> AppTy (go t1) (go t2)
    LitTy l -> LitTy l
    AnnType a t -> AnnType a (go t)

-- | Apply a substitution to a term
substTm :: Distinct o => Subst i o -> Term i -> Term o
substTm s@(UnsafeSubst _ tys tms fr) tm0
  | not fr && IntMap.null tys && IntMap.null tms = coerceTm tm0
  | otherwise = go tm0
 where
  goTy = substTy s
  go tm = case tm of
    Var occ ty n -> case IntMap.lookup (tmNameRaw n) tms of
      Just tm' -> tm'
      Nothing
        | IntMap.null tys && not fr -> coerceTm tm
        -- The type of the occurrence may mention a substituted variable, see
        -- the module header
        | otherwise -> Var occ (goTy ty) (coerceTmName n)
    GlobalVar g -> GlobalVar g
    Data dc -> Data dc
    Literal l -> Literal l
    Prim p -> Prim p
    Lam b e -> substIdBinder s b $ \s' b' -> Lam b' (substTm s' e)
    TyLam b e -> substTyBinder s b $ \s' b' -> TyLam b' (substTm s' e)
    App e1 e2 -> App (go e1) (go e2)
    TyApp e ty -> TyApp (go e) (goTy ty)
    Let bs e -> substBind s bs $ \s' bs' -> Let bs' (substTm s' e)
    Case e ty alts -> Case (go e) (goTy ty) (map (substAlt s) alts)
    Cast e t1 t2 -> Cast (go e) (goTy t1) (goTy t2)
    Tick t e -> Tick (substTickInfo s t) (go e)

-- | Apply a substitution to a case alternative
substAlt :: Distinct o => Subst i o -> Alt i -> Alt o
substAlt s (Alt p e) = substPat s p $ \s' p' -> Alt p' (substTm s' e)

-- | Apply a substitution to a tick
substTickInfo :: Distinct o => Subst i o -> TickInfo i -> TickInfo o
substTickInfo s = \case
  SrcSpan sp -> SrcSpan sp
  NameMod m ty -> NameMod m (substTy s ty)
  DeDup -> DeDup
  NoDeDup -> NoDeDup
  Attributes ty e -> Attributes (substTy s ty) (substTm s e)

-- Rename a binder if it may clash with the scope, or always when asked to
refresh
  :: Distinct o
  => Scope o -> NameBinder i i' -> Bool
  -> (forall o'. DExt o o' => NameBinder o o' -> r) -> r
refresh sc b fr cont
  | fr = withFresh sc cont
  | otherwise = withRefreshed sc (nameOf b) cont
{-# INLINE refresh #-}

-- | Go under a type binder: substitute in its kind, and rename it if it
-- clashes with the output scope
substTyBinder
  :: Distinct o
  => Subst i o
  -> TyBinder i i'
  -> (forall o'. DExt o o' => Subst i' o' -> TyBinder o o' -> r)
  -> r
substTyBinder s@(UnsafeSubst _ _ _ fr) b = substTyBinderK fr s b (substTy s (tyBinderKind b))

-- | 'substTyBinder', given the kind of the binder, substituted. Renames the
-- binder if it clashes with the output scope, or always when asked to.
substTyBinderK
  :: Distinct o
  => Bool
  -> Subst i o
  -> TyBinder i i'
  -> Kind o
  -> (forall o'. DExt o o' => Subst i' o' -> TyBinder o o' -> r)
  -> r
substTyBinderK always (UnsafeSubst sc tys tms fr) (TyBinder occ _ b) k' cont =
  refresh sc (unsortTyNameBinder b) always $ \b' ->
    let new = nameId (nameOf b')
        -- Trusted: b' is fresh, see the module header
        bndr' = TyBinder occ k' (UnsafeTyNameBinder b')
        sc' = extendScope b' sc
        tms' = IntMap.delete old (sink1 tms)
    in if old == new && IntMap.null tys
         then cont (UnsafeSubst sc' (sink1 tys) tms' fr) bndr'
         else cont
           (UnsafeSubst sc'
             (IntMap.insert old (VarTy occ (sink k') (tyNameOf bndr')) (sink1 tys))
             tms'
             fr)
           bndr'
 where
  old = tyNameBinderRaw b

-- | Go under a term binder: substitute in its type, and rename it if it
-- clashes with the output scope
substIdBinder
  :: Distinct o
  => Subst i o
  -> IdBinder i i'
  -> (forall o'. DExt o o' => Subst i' o' -> IdBinder o o' -> r)
  -> r
substIdBinder s@(UnsafeSubst _ _ _ fr) b = substIdBinderK fr s b (substTy s (idBinderType b))

-- | 'substIdBinder', given the type of the binder, substituted. Renames the
-- binder if it clashes with the output scope, or always when asked to.
substIdBinderK
  :: Distinct o
  => Bool
  -> Subst i o
  -> IdBinder i i'
  -> Type o
  -> (forall o'. DExt o o' => Subst i' o' -> IdBinder o o' -> r)
  -> r
substIdBinderK always (UnsafeSubst sc tys tms fr) (IdBinder occ _ b) ty' cont =
  refresh sc (unsortTmNameBinder b) always $ \b' ->
    let new = nameId (nameOf b')
        -- Trusted: b' is fresh, see the module header
        bndr' = IdBinder occ ty' (UnsafeTmNameBinder b')
        sc' = extendScope b' sc
        tys' = IntMap.delete old (sink1 tys)
    in if old == new && IntMap.null tys
         then cont (UnsafeSubst sc' tys' (IntMap.delete old (sink1 tms)) fr) bndr'
         else cont
           (UnsafeSubst sc'
             tys'
             (IntMap.insert old (Var occ (sink ty') (tmNameOf bndr')) (sink1 tms))
             fr)
           bndr'
 where
  old = tmNameBinderRaw b

-- | Go under a telescope of binders
substTele
  :: Distinct o
  => (forall x x' y r'. Distinct y => Subst x y -> b x x' -> (forall y'. DExt y y' => Subst x' y' -> b y y' -> r') -> r')
  -- ^ How to go under a single binder: 'substTyBinder' or 'substIdBinder'
  -> Subst i o
  -> Tele b i i'
  -> (forall o'. DExt o o' => Subst i' o' -> Tele b o o' -> r)
  -> r
substTele _ s TNil cont = cont s TNil
substTele substB s (TCons b bs) cont =
  substB s b $ \s1 b' ->
    substTele substB s1 bs $ \s2 bs' ->
      cont s2 (TCons b' bs')

-- | Go under a pattern
substPat
  :: Distinct o
  => Subst i o
  -> Pat i i'
  -> (forall o'. DExt o o' => Subst i' o' -> Pat o o' -> r)
  -> r
substPat s p cont = case p of
  DataPat dc tvs ids ->
    substTele substTyBinder s tvs $ \s1 tvs' ->
      substTele substIdBinder s1 ids $ \s2 ids' ->
        cont s2 (DataPat dc tvs' ids')
  LitPat l -> cont s (LitPat l)
  DefaultPat -> cont s DefaultPat

-- | Go under let-bindings, substituting in their right-hand sides
substBind
  :: Distinct o
  => Subst i o
  -> Bind i i'
  -> (forall o'. DExt o o' => Subst i' o' -> Bind o o' -> r)
  -> r
substBind s bs cont = case bs of
  NonRec b e ->
    let e' = substTm s e in
    substIdBinder s b $ \s' b' -> cont s' (NonRec b' e')
  Rec bndrs es ->
    substTele substIdBinder s bndrs $ \s' bndrs' ->
      cont s' (Rec bndrs' (map (substTm s') es))

-- | Rename every binder in a value to the name just above the watermark of
-- the scope at the binder. Unlike conversion from Clash Core, which numbers
-- the binders of a term depth-first, this gives binders in different subterms
-- the same name: e.g. both binders in @f (\x -> x) (\y -> y)@. See
-- 'freshenDistinct' to give every binder a different name.
freshen :: (Distinct n, HasSubst e) => Scope n -> e n -> e n
freshen sc = subst (UnsafeSubst sc IntMap.empty IntMap.empty True)

-- | Rename every binder in a term to a distinct fresh name, like conversion
-- from Clash Core does ("Clash.Core.Scoped.Convert"): it numbers the binders
-- in depth-first order, from just above the watermark of the scope. So no two
-- binders in the result have the same name, and none clashes with the scope.
-- E.g. the binders in @f (\x -> x) (\y -> y)@ get two different names, whereas
-- 'freshen' gives them the same one.
--
-- This includes the binders of the types in the term, except those of the
-- types of variable occurrences, which are those of their binders (or of the
-- free variables of the term). The order is the one of conversion: a binder
-- after the binders in its type or kind, a non-recursive let-binding after its
-- right-hand side, and the binders of a recursive let-binding before their
-- right-hand sides. So a term converted from Clash Core keeps its names.
freshenDistinct :: Distinct n => Scope n -> Term n -> Term n
freshenDistinct sc tm = case distinctTm (idSubst sc) tm of
  Distinctly tm' _ -> tm'

-- | A value with distinct binders, and the watermark to continue with to its
-- right: the largest name it binds, or the watermark it started from if it
-- binds none. See 'freshenDistinct'.
data Distinctly a = Distinctly a !Int

-- Raise the watermark of the output scope to the largest name bound so far
raiseSubst :: Int -> Subst i o -> Subst i o
raiseSubst w (UnsafeSubst sc tys tms fr) = UnsafeSubst (raiseWatermark w sc) tys tms fr
{-# INLINE raiseSubst #-}

-- The largest name bound so far, see 'raiseSubst'
watermarkOf :: Subst i o -> Int
watermarkOf = scopeWatermark . substScope
{-# INLINE watermarkOf #-}

-- Freshen two values in order: the second continues above the names of the
-- first
distinctly2
  :: Subst i o
  -> (Subst i o -> Distinctly a)
  -> (Subst i o -> Distinctly b)
  -> (a -> b -> c)
  -> Distinctly c
distinctly2 s f g k = case f s of
  Distinctly a w1 -> case g (raiseSubst w1 s) of
    Distinctly b w2 -> Distinctly (k a b) w2
{-# INLINE distinctly2 #-}

-- Freshen values in order, see 'distinctly2'
distinctlyList :: Subst i o -> (Subst i o -> x -> Distinctly a) -> [x] -> Distinctly [a]
distinctlyList s0 f = go s0
 where
  go s [] = Distinctly [] (watermarkOf s)
  go s (x:xs) = distinctly2 s (`f` x) (`go` xs) (:)

distinctTy :: Distinct o => Subst i o -> Type i -> Distinctly (Type o)
distinctTy s ty = case ty of
  VarTy {} -> Distinctly (substTy s ty) (watermarkOf s)
  ConstTy c -> Distinctly (ConstTy c) (watermarkOf s)
  ForAllTy b t -> distinctTyBinder s b $ \s' b' -> case distinctTy s' t of
    Distinctly t' w -> Distinctly (ForAllTy b' t') w
  AppTy t1 t2 -> distinctly2 s (`distinctTy` t1) (`distinctTy` t2) AppTy
  LitTy l -> Distinctly (LitTy l) (watermarkOf s)
  AnnType a t -> case distinctTy s t of
    Distinctly t' w -> Distinctly (AnnType a t') w

distinctTyBinder
  :: Distinct o
  => Subst i o
  -> TyBinder i i'
  -> (forall o'. DExt o o' => Subst i' o' -> TyBinder o o' -> r)
  -> r
distinctTyBinder s b cont = case distinctTy s (tyBinderKind b) of
  Distinctly k' w -> substTyBinderK True (raiseSubst w s) b k' cont

distinctIdBinder
  :: Distinct o
  => Subst i o
  -> IdBinder i i'
  -> (forall o'. DExt o o' => Subst i' o' -> IdBinder o o' -> r)
  -> r
distinctIdBinder s b cont = case distinctTy s (idBinderType b) of
  Distinctly ty' w -> substIdBinderK True (raiseSubst w s) b ty' cont

distinctTm :: Distinct o => Subst i o -> Term i -> Distinctly (Term o)
distinctTm s tm = case tm of
  Var {} -> Distinctly (substTm s tm) w0
  GlobalVar g -> Distinctly (GlobalVar g) w0
  Data dc -> Distinctly (Data dc) w0
  Literal l -> Distinctly (Literal l) w0
  Prim p -> Distinctly (Prim p) w0
  Lam b e -> distinctIdBinder s b $ \s' b' -> case distinctTm s' e of
    Distinctly e' w -> Distinctly (Lam b' e') w
  TyLam b e -> distinctTyBinder s b $ \s' b' -> case distinctTm s' e of
    Distinctly e' w -> Distinctly (TyLam b' e') w
  App e1 e2 -> distinctly2 s (`distinctTm` e1) (`distinctTm` e2) App
  TyApp e ty -> distinctly2 s (`distinctTm` e) (`distinctTy` ty) TyApp
  Let (NonRec b x) e -> case distinctTm s x of
    Distinctly x' w1 -> distinctIdBinder (raiseSubst w1 s) b $ \s' b' ->
      case distinctTm s' e of
        Distinctly e' w2 -> Distinctly (Let (NonRec b' x') e') w2
  Let (Rec bs xs) e -> substTele distinctIdBinder s bs $ \s' bs' ->
    distinctly2 s' (\s1 -> distinctlyList s1 distinctTm xs) (`distinctTm` e) $ \xs' e' ->
      Let (Rec bs' xs') e'
  Case e ty alts ->
    distinctly2 s (`distinctTm` e) (\s1 -> distinctly2 s1 (`distinctTy` ty) (\s2 -> distinctlyList s2 distinctAlt alts) (,)) $
      \e' (ty', alts') -> Case e' ty' alts'
  Cast e t1 t2 ->
    distinctly2 s (`distinctTm` e) (\s1 -> distinctly2 s1 (`distinctTy` t1) (`distinctTy` t2) (,)) $
      \e' (t1', t2') -> Cast e' t1' t2'
  Tick t e -> distinctly2 s (`distinctTick` t) (`distinctTm` e) Tick
 where
  w0 = watermarkOf s

distinctAlt :: Distinct o => Subst i o -> Alt i -> Distinctly (Alt o)
distinctAlt s (Alt p e) = case p of
  DataPat dc tvs ids ->
    substTele distinctTyBinder s tvs $ \s1 tvs' ->
      substTele distinctIdBinder s1 ids $ \s2 ids' -> case distinctTm s2 e of
        Distinctly e' w -> Distinctly (Alt (DataPat dc tvs' ids') e') w
  LitPat l -> case distinctTm s e of
    Distinctly e' w -> Distinctly (Alt (LitPat l) e') w
  DefaultPat -> case distinctTm s e of
    Distinctly e' w -> Distinctly (Alt DefaultPat e') w

distinctTick :: Distinct o => Subst i o -> TickInfo i -> Distinctly (TickInfo o)
distinctTick s = \case
  SrcSpan sp -> Distinctly (SrcSpan sp) (watermarkOf s)
  NameMod m ty -> case distinctTy s ty of
    Distinctly ty' w -> Distinctly (NameMod m ty') w
  DeDup -> Distinctly DeDup (watermarkOf s)
  NoDeDup -> Distinctly NoDeDup (watermarkOf s)
  Attributes ty e -> distinctly2 s (`distinctTy` ty) (`distinctTm` e) Attributes

-- * Going under binders
--
-- Going under a binder is substituting the identity, which only renames
-- binders that may clash with the scope. The functions below first check
-- whether all of the binders are surely fresh, and if they are, reuse the
-- binders and their scope as they are.

-- | Go under a type binder, renaming it (and its scope) only if it clashes
-- with the scope
openTyBinder
  :: (Distinct n, HasSubst e)
  => Scope n
  -> TyBinder n l
  -> e l
  -> (forall l'. DExt n l' => Scope l' -> TyBinder n l' -> e l' -> r)
  -> r
openTyBinder sc b e cont =
  withFreshRaw sc (tyNameRaw (tyNameOf b))
    -- Trusted: see the module header
    (\sc' -> cont sc' (unsafeCoerce b) (unsafeCoerce e))
    (substTyBinder (idSubst sc) b $ \s' b' -> cont (substScope s') b' (subst s' e))
{-# INLINE openTyBinder #-}

-- | Go under a term binder, renaming it (and its scope) only if it clashes
-- with the scope
openIdBinder
  :: (Distinct n, HasSubst e)
  => Scope n
  -> IdBinder n l
  -> e l
  -> (forall l'. DExt n l' => Scope l' -> IdBinder n l' -> e l' -> r)
  -> r
openIdBinder sc b e cont =
  withFreshRaw sc (tmNameRaw (tmNameOf b))
    -- Trusted: see the module header
    (\sc' -> cont sc' (unsafeCoerce b) (unsafeCoerce e))
    (substIdBinder (idSubst sc) b $ \s' b' -> cont (substScope s') b' (subst s' e))
{-# INLINE openIdBinder #-}

-- | Go under the pattern of a case alternative, renaming the variables it
-- binds (and their scope) only if they clash with the scope
openAlt
  :: Distinct n
  => Scope n
  -> Alt n
  -> (forall l. DExt n l => Scope l -> Pat n l -> Term l -> r)
  -> r
openAlt sc (Alt p e) cont = case p of
  DataPat _ tvs ids ->
    withFreshRaws sc
      (\step done ->
         teleRawsF tyNameBinderRaw tyBinderName tvs step
           (teleRawsF tmNameBinderRaw idBinderName ids step done))
      -- Trusted: see the module header
      (\sc' -> cont sc' (unsafeCoerce p) (unsafeCoerce e))
      (substPat (idSubst sc) p $ \s' p' -> cont (substScope s') p' (substTm s' e))
  LitPat {} -> cont sc p e
  DefaultPat -> cont sc p e
{-# INLINE openAlt #-}

-- | Go under let-bindings, renaming the variables they bind (and their scope)
-- only if they clash with the scope
openBind
  :: (Distinct n, HasSubst e)
  => Scope n
  -> Bind n l
  -> e l
  -> (forall l'. DExt n l' => Scope l' -> Bind n l' -> e l' -> r)
  -> r
openBind sc bs e cont =
  withFreshRaws sc bindRaws
    -- Trusted: see the module header
    (\sc' -> cont sc' (unsafeCoerce bs) (unsafeCoerce e))
    (substBind (idSubst sc) bs $ \s' bs' -> cont (substScope s') bs' (subst s' e))
 where
  bindRaws :: (Int -> (Int -> a) -> Int -> a) -> (Int -> a) -> Int -> a
  bindRaws step done = case bs of
    NonRec b _ -> step (tmNameRaw (tmNameOf b)) done
    Rec bndrs _ -> teleRawsF tmNameBinderRaw idBinderName bndrs step done
{-# INLINE openBind #-}

-- | The raw names bound by a telescope, as a fold for 'withFreshRaws'
teleRawsF
  :: forall b sb n l acc a
   . (forall x y. sb x y -> Int)
  -> (forall x y. b x y -> sb x y)
  -> Tele b n l
  -> (Int -> (acc -> a) -> acc -> a)
  -> (acc -> a)
  -> acc
  -> a
teleRawsF raw binder tele step done = go tele
 where
  go :: Tele b x y -> acc -> a
  go TNil = done
  go (TCons b bs) = step (raw (binder b)) (go bs)
{-# INLINE teleRawsF #-}

-- * Fresh binders

-- | Create a type variable binder with a fresh name
withFreshTyBinder
  :: Distinct n
  => Scope n
  -> C.Name C.Type
  -- ^ Human readable name. Its unique is not used.
  -> Kind n
  -> (forall l. DExt n l => Scope l -> TyBinder n l -> r)
  -> r
withFreshTyBinder sc occ k cont =
  withFresh sc $ \b ->
    -- Trusted: b is fresh, see the module header
    cont (extendScope b sc) (TyBinder occ k (UnsafeTyNameBinder b))
{-# INLINE withFreshTyBinder #-}

-- | Create a term variable binder with a fresh name
withFreshIdBinder
  :: Distinct n
  => Scope n
  -> C.Name C.Term
  -- ^ Human readable name. Its unique is not used.
  -> Type n
  -> (forall l. DExt n l => Scope l -> IdBinder n l -> r)
  -> r
withFreshIdBinder sc occ ty cont =
  withFresh sc $ \b ->
    -- Trusted: b is fresh, see the module header
    cont (extendScope b sc) (IdBinder occ ty (UnsafeTmNameBinder b))
{-# INLINE withFreshIdBinder #-}

-- | Create a telescope of term variable binders with fresh names, e.g. for
-- the fields of a 'DataPat'. Also gives the variables they bind.
withFreshIdBinders
  :: forall n r
   . Distinct n
  => Scope n
  -> [(C.Name C.Term, Type n)]
  -- ^ Human readable names (whose uniques are not used) and types
  -> (forall l. DExt n l => Scope l -> Tele IdBinder n l -> [Term l] -> r)
  -> r
withFreshIdBinders sc0 bs0 cont = go sc0 [] bs0 cont
 where
  -- The types of the remaining binders are in scope n, and are sunk into x.
  -- The variables bound so far are in reverse order.
  go
    :: DExt n x
    => Scope x
    -> [Term x]
    -> [(C.Name C.Term, Type n)]
    -> (forall l. DExt x l => Scope l -> Tele IdBinder x l -> [Term l] -> r')
    -> r'
  go sc vs [] k = k sc TNil (reverse vs)
  go sc vs ((occ, ty):bs) k =
    withFreshIdBinder sc occ (sink ty) $ \sc1 b ->
      go sc1 (idBinderVar b : sink1 vs) bs $ \sc2 bs' vs' -> k sc2 (TCons b bs') vs'

-- * Moving out of the scope of binders

-- | Move a term out of the scope of binders, if none of the variables they
-- bind occur in it
unsinkTerm :: NameBinders n l -> Term l -> Maybe (Term n)
unsinkTerm bs e
  | anyBoundOccursIn bs e = Nothing
  -- Trusted: see the module header
  | otherwise = Just (unsafeCoerce e)

-- | Move a type out of the scope of binders, if none of the variables they
-- bind occur in it
unsinkType :: NameBinders n l -> Type l -> Maybe (Type n)
unsinkType bs ty
  | anyBoundOccursInType bs ty = Nothing
  -- Trusted: see the module header
  | otherwise = Just (unsafeCoerce ty)

-- | Move a term out of the scope of a type variable binder, if the variable
-- doesn't occur in it
unsinkTyBinder :: TyBinder n l -> Term l -> Maybe (Term n)
unsinkTyBinder b e
  | tyNameOccursIn (tyNameOf b) e = Nothing
  -- Trusted: see the module header
  | otherwise = Just (unsafeCoerce e)

-- | Move a term out of the scope of a term variable binder, if the variable
-- doesn't occur in it
unsinkIdBinder :: IdBinder n l -> Term l -> Maybe (Term n)
unsinkIdBinder b e
  | tmNameOccursIn (tmNameOf b) e = Nothing
  -- Trusted: see the module header
  | otherwise = Just (unsafeCoerce e)

-- | Move the right-hand side of a case alternative out of the scope of its
-- pattern, if none of the variables the pattern binds occur in it
unsinkAlt :: Pat n l -> Term l -> Maybe (Term n)
unsinkAlt p e
  | patBindersOccurIn p e = Nothing
  -- Trusted: see the module header
  | otherwise = Just (unsafeCoerce e)

-- | Move a type out of the scope of a term variable binder. Always possible:
-- a type only mentions type variables.
strengthenTyId :: IdBinder n l -> Type l -> Type n
-- Trusted: see the module header
strengthenTyId _ = unsafeCoerce
{-# INLINE strengthenTyId #-}

-- | Move a type out of the scope of term variable binders, see
-- 'strengthenTyId'
strengthenTyIds :: Tele IdBinder n l -> Type l -> Type n
-- Trusted: see the module header
strengthenTyIds _ = unsafeCoerce
{-# INLINE strengthenTyIds #-}

-- | Move a type out of the scope of a pattern, if the type variables the
-- pattern binds don't occur in it
strengthenTyPat :: Pat n l -> Type l -> Maybe (Type n)
strengthenTyPat p ty = case p of
  DataPat _ TNil ids -> Just (strengthenTyIds ids ty)
  DataPat {} -> unsinkType (patNameBinders p) ty
  LitPat _ -> Just ty
  DefaultPat -> Just ty

-- | The types of the binders of a telescope of term variable binders, moved
-- into the scope the telescope extends. Always possible, see
-- 'strengthenTyId'. ('strengthenTyIds' would need the telescope of the
-- binders before each binder.)
teleIdBinderTypes :: Tele IdBinder n l -> [Type n]
teleIdBinderTypes = go
 where
  go :: Tele IdBinder x l -> [Type n]
  go TNil = []
  -- Trusted: see the module header
  go (TCons b bs) = unsafeCoerce (idBinderType b) : go bs

-- | Move a term into the empty scope, if it has no free local variables of
-- either sort, e.g. to make it the body of a global binding. 'sinkClosed'
-- moves it back.
closeTerm :: Term n -> Maybe (Term 'VoidS)
closeTerm e
  | hasFreeVars e = Nothing
  -- Trusted: see the module header
  | otherwise = Just (unsafeCoerce e)

-- | Move a type into the empty scope, if it has no free type variables. See
-- 'closeTerm'.
closeType :: Type n -> Maybe (Type 'VoidS)
closeType ty
  | hasFreeVarsType ty = Nothing
  -- Trusted: see the module header
  | otherwise = Just (unsafeCoerce ty)

-- * Variables of binders

-- | The variables that a telescope binds, as terms in the scope it extends
-- to. See 'idBinderVar' for a single binder.
teleIdVars :: Distinct l => Tele IdBinder n l -> [Term l]
teleIdVars = unsafeTeleIdVars

-- | The type variables and the term variables that a pattern binds, as types
-- and terms in the scope of its alternative
patVars :: Distinct l => Pat n l -> ([Type l], [Term l])
patVars = \case
  DataPat _ tvs ids -> (unsafeTeleTyVars tvs, unsafeTeleIdVars ids)
  LitPat _ -> ([], [])
  DefaultPat -> ([], [])

-- | The variables of the binders of a telescope, in scope @l@. Trusted: only
-- use it when @l@ is the scope the telescope extends to, or extends that one,
-- see the module header.
unsafeTeleIdVars :: Tele IdBinder x y -> [Term l]
unsafeTeleIdVars TNil = []
unsafeTeleIdVars (TCons b bs) =
  Var (idBinderOcc b) (unsafeCoerce (idBinderType b)) (unsafeCoerce (tmNameOf b))
    : unsafeTeleIdVars bs

-- | Like 'unsafeTeleIdVars', for type variables
unsafeTeleTyVars :: Tele TyBinder x y -> [Type l]
unsafeTeleTyVars TNil = []
unsafeTeleTyVars (TCons b bs) =
  VarTy (tyBinderOcc b) (unsafeCoerce (tyBinderKind b)) (unsafeCoerce (tyNameOf b))
    : unsafeTeleTyVars bs

-- * Closing over variables

-- | Free variables found so far by 'freeVarOccs': the raw names of the type
-- and term variables, and their first occurrences, in reverse order
data Occs (n :: S) = Occs !IntSet !IntSet [Type n] [Term n]

-- | The free type variables and the free term variables of a term, as their
-- first occurrences ('VarTy' and 'Var'), which carry their kinds and types.
-- In the order of their first occurrences, like the free variables that
-- 'Clash.Core.FreeVars.freeLocalVars' visits: depth-first, left to right, a
-- variable before the variables in its type or kind, and the type of a
-- binder before its scope. Includes the variables of the types in the term,
-- like 'freeVars'.
--
-- Errors if the type or kind of a first occurrence mentions a type variable
-- bound in the term: that is not a type of the scope of the term, which an
-- occurrence built by hand may have.
freeVarOccs :: HasCallStack => Term n -> ([Type n], [Term n])
freeVarOccs tm =
  case occsTm IntSet.empty tm (Occs IntSet.empty IntSet.empty [] []) of
    Occs _ _ tys tms -> (reverse tys, reverse tms)

-- | The free type variables of a type, as their first occurrences, see
-- 'freeVarOccs'
freeTyVarOccsOfType :: HasCallStack => Type n -> [Type n]
freeTyVarOccsOfType ty =
  case occsTy IntSet.empty ty (Occs IntSet.empty IntSet.empty [] []) of
    Occs _ _ tys _ -> reverse tys

-- | Whether a type mentions one of the given raw names
mentions :: IntSet -> Type x -> Bool
mentions bound ty =
  not (IntSet.null bound)
    && any ((`IntSet.member` bound) . tyNameRaw) (nameSetToList (freeTyVarsOfType ty))

-- | Add the free variables of a type, given the raw names of the variables
-- bound on the path to it
occsTy :: HasCallStack => IntSet -> Type x -> Occs n -> Occs n
occsTy bound ty acc@(Occs seenTy seenTm tys tms) = case ty of
  VarTy occ k nm
    | let r = tyNameRaw nm
    , not (IntSet.member r bound) && not (IntSet.member r seenTy)
    -> if mentions bound k
         then error ("freeVarOccs: the kind of type variable " <> show occ
                     <> " mentions a type variable bound in the term")
         -- Trusted: a free occurrence, see the module header
         else occsTy bound k (Occs (IntSet.insert r seenTy) seenTm (unsafeCoerce ty : tys) tms)
    | otherwise -> occsTy bound k acc
  ConstTy {} -> acc
  ForAllTy b t ->
    occsTy (IntSet.insert (tyNameRaw (tyNameOf b)) bound) t (occsTy bound (tyBinderKind b) acc)
  AppTy t1 t2 -> occsTy bound t2 (occsTy bound t1 acc)
  LitTy {} -> acc
  AnnType _ t -> occsTy bound t acc

-- | Add the free variables of a term, given the raw names of the variables
-- bound on the path to it
occsTm :: HasCallStack => IntSet -> Term x -> Occs n -> Occs n
occsTm bound tm acc@(Occs seenTy seenTm tys tms) = case tm of
  Var occ ty nm
    | let r = tmNameRaw nm
    , not (IntSet.member r bound) && not (IntSet.member r seenTm)
    -> if mentions bound ty
         then error ("freeVarOccs: the type of variable " <> show occ
                     <> " mentions a type variable bound in the term")
         -- Trusted: a free occurrence, see the module header
         else occsTy bound ty (Occs seenTy (IntSet.insert r seenTm) tys (unsafeCoerce tm : tms))
    | otherwise -> occsTy bound ty acc
  GlobalVar {} -> acc
  Data {} -> acc
  Literal {} -> acc
  Prim {} -> acc
  Lam b e -> occsTm (bind (tmNameRaw (tmNameOf b))) e (occsTy bound (idBinderType b) acc)
  TyLam b e -> occsTm (bind (tyNameRaw (tyNameOf b))) e (occsTy bound (tyBinderKind b) acc)
  App e1 e2 -> occsTm bound e2 (occsTm bound e1 acc)
  TyApp e ty -> occsTy bound ty (occsTm bound e acc)
  Let (NonRec b x) e ->
    occsTm (bind (tmNameRaw (tmNameOf b))) e
      (occsTm bound x (occsTy bound (idBinderType b) acc))
  Let (Rec bs xs) e ->
    -- The type of a binder is in the scope of the binders before it, the
    -- right-hand sides and the body are in the scope of all of them
    let (bounds, boundAll) = teleBounds bound bs
        step a (bnd, (SomeType ty, x)) = occsTm boundAll x (occsTy bnd ty a)
    in occsTm boundAll e (List.foldl' step acc (zip bounds (zip (teleTypes bs) xs)))
  Case e ty alts ->
    List.foldl' (flip (occsAlt bound)) (occsTy bound ty (occsTm bound e acc)) alts
  Cast e t1 t2 -> occsTy bound t2 (occsTy bound t1 (occsTm bound e acc))
  Tick t e -> occsTm bound e (occsTick bound t acc)
 where
  bind r = IntSet.insert r bound

-- | Add the free variables of a case alternative
occsAlt :: HasCallStack => IntSet -> Alt x -> Occs n -> Occs n
occsAlt bound (Alt p e) acc = case p of
  DataPat _ tvs ids ->
    let (acc1, bound1) = occsTyTele bound tvs acc
        (idBounds, boundAll) = teleBounds bound1 ids
        step a (bnd, SomeType ty) = occsTy bnd ty a
    in occsTm boundAll e (List.foldl' step acc1 (zip idBounds (teleTypes ids)))
  LitPat _ -> occsTm bound e acc
  DefaultPat -> occsTm bound e acc
 where
  occsTyTele :: IntSet -> Tele TyBinder x' y' -> Occs n' -> (Occs n', IntSet)
  occsTyTele bnd TNil a = (a, bnd)
  occsTyTele bnd (TCons b bs) a =
    occsTyTele (IntSet.insert (tyNameRaw (tyNameOf b)) bnd) bs (occsTy bnd (tyBinderKind b) a)

-- | Add the free variables of a tick
occsTick :: HasCallStack => IntSet -> TickInfo x -> Occs n -> Occs n
occsTick bound t acc = case t of
  NameMod _ ty -> occsTy bound ty acc
  Attributes ty e -> occsTm bound e (occsTy bound ty acc)
  _ -> acc

-- | For every binder of a telescope of term variable binders, the raw names
-- bound on the path to its type; and those bound on the path to the scope of
-- the telescope
teleBounds :: IntSet -> Tele IdBinder x y -> ([IntSet], IntSet)
teleBounds bound TNil = ([], bound)
teleBounds bound (TCons b bs) =
  let (rest, boundAll) = teleBounds (IntSet.insert (tmNameRaw (tmNameOf b)) bound) bs
  in (bound : rest, boundAll)

-- | The types of the binders of a telescope, each in its own scope
teleTypes :: Tele IdBinder x y -> [SomeType]
teleTypes TNil = []
teleTypes (TCons b bs) = SomeType (idBinderType b) : teleTypes bs

-- | A type in some scope
data SomeType where
  SomeType :: Type x -> SomeType

-- | Moving values of scope @n@ that only mention some of its variables into
-- the scope @l@ that binds just those variables, see 'closeOver'
data Closure (n :: S) (l :: S) = Closure
  { closureType :: Type n -> Maybe (Type l)
  -- ^ Move a type into scope @l@, if its free type variables are among the
  -- variables of @l@
  , closureTerm :: Term n -> Maybe (Term l)
  -- ^ Move a term into scope @l@, if its free variables (including those of
  -- its types) are among the variables of @l@
  }

-- | Bind some of the variables of scope @n@ in the empty scope, e.g. to
-- abstract a term over its free variables. Gives a telescope of type variable
-- binders, followed by a telescope of term variable binders, that bind the
-- given variables with their human readable names, raw names, and the kinds
-- and types of the given occurrences; the scope @l@ these extend the empty
-- scope to; and a 'Closure' that moves values of @n@ that only mention these
-- variables into @l@. E.g. abstracting a term over its free variables, given
-- in an order 'closeOver' accepts:
--
-- @
-- 'closeOver' tvs ids $ \\_ tvTele idTele cl ->
--   'Clash.Core.Scoped.Term.mkTyLams' ('Clash.Core.Scoped.Term.mkLams' ('closureTerm' cl e) idTele) tvTele
-- @
--
-- 'Nothing' if an occurrence is not a variable, if a variable is given twice,
-- if the kind of a type variable mentions a type variable that is not before
-- it in the list, or if the type of a term variable mentions a type variable
-- that is not in the list.
closeOver
  :: [Type n]
  -- ^ Occurrences ('VarTy') of the type variables to bind
  -> [Term n]
  -- ^ Occurrences ('Var') of the term variables to bind
  -> (forall i l. (DExt 'VoidS i, DExt i l)
        => Scope l -> Tele TyBinder 'VoidS i -> Tele IdBinder i l -> Closure n l -> r)
  -> Maybe r
closeOver tvs ids cont = do
  tvOccs <- traverse tyOcc tvs
  idOccs <- traverse tmOcc ids
  let tvRaws = [r | (_, _, r) <- tvOccs]
      idRaws = [r | (_, _, r) <- idOccs]
      tvSet = IntSet.fromList tvRaws
      idSet = IntSet.fromList idRaws
      allRaws = tvRaws ++ idRaws
      within s = all ((`IntSet.member` s) . tyNameRaw) . nameSetToList
      kindsOk =
        and (zipWith (\(_, k, _) before -> within before (freeTyVarsOfType k))
                     tvOccs
                     (scanl (flip IntSet.insert) IntSet.empty tvRaws))
      typesOk = all (\(_, ty, _) -> within tvSet (freeTyVarsOfType ty)) idOccs
  if IntSet.size (IntSet.union tvSet idSet) /= length allRaws || not kindsOk || not typesOk
    then Nothing
    else
      -- Trusted: see the module header. The scopes are instantiated at
      -- 'VoidS, which the continuation can't observe.
      let tyTele :: Tele TyBinder 'VoidS 'VoidS
          tyTele = foldr
            (\(occ, k, r) rest -> TCons
               (TyBinder occ (unsafeCoerce k) (UnsafeTyNameBinder (UnsafeNameBinder (UnsafeName r))))
               rest)
            TNil tvOccs
          idTele :: Tele IdBinder 'VoidS 'VoidS
          idTele = foldr
            (\(occ, ty, r) rest -> TCons
               (IdBinder occ (unsafeCoerce ty) (UnsafeTmNameBinder (UnsafeNameBinder (UnsafeName r))))
               rest)
            TNil idOccs
          closure = Closure
            { closureType = \ty ->
                if within tvSet (freeTyVarsOfType ty) then Just (unsafeCoerce ty) else Nothing
            , closureTerm = \tm ->
                if within tvSet (freeTyVars tm)
                     && all ((`IntSet.member` idSet) . tmNameRaw) (nameSetToList (freeTmVars tm))
                  then Just (unsafeCoerce tm)
                  else Nothing
            }
          sc :: Scope 'VoidS
          sc = UnsafeScope (List.foldl' max 0 allRaws)
      in Just (cont sc tyTele idTele closure)
 where
  tyOcc :: Type n -> Maybe (C.Name C.Type, Kind n, Int)
  tyOcc = \case
    VarTy occ k nm -> Just (occ, k, tyNameRaw nm)
    _ -> Nothing

  tmOcc :: Term n -> Maybe (C.Name C.Term, Type n, Int)
  tmOcc = \case
    Var occ ty nm -> Just (occ, ty, tmNameRaw nm)
    _ -> Nothing

-- * Global variables

-- | Replace global variables by the closed terms of a t'GlobalEnv', see
-- 'Clash.Core.Subst.extendGblSubstList'. It doesn't go under binders, as a
-- closed term can't capture variables, nor mention any.
--
-- Trusted: a closed term is a term in every scope, so it is coerced into the
-- scope of the occurrence it replaces ('sinkClosed' without the need for a
-- t'Distinct' scope, which unopened binders don't have).
substGlobals :: GlobalEnv (Term 'VoidS) -> Term n -> Term n
substGlobals env e0
  | nullGlobalEnv env = e0
  | otherwise = go e0
 where
  go :: Term x -> Term x
  go = \case
    e@(GlobalVar g) -> maybe e coerceTm (lookupGlobalEnv g env)
    e@Var {} -> e
    e@Data {} -> e
    e@Literal {} -> e
    e@Prim {} -> e
    Lam b e -> Lam b (go e)
    TyLam b e -> TyLam b (go e)
    App e1 e2 -> App (go e1) (go e2)
    TyApp e ty -> TyApp (go e) ty
    Let (NonRec b x) e -> Let (NonRec b (go x)) (go e)
    Let (Rec bs xs) e -> Let (Rec bs (map go xs)) (go e)
    Case e ty alts -> Case (go e) ty [Alt p (go x) | Alt p x <- alts]
    Cast e ty1 ty2 -> Cast (go e) ty1 ty2
    Tick (Attributes ty a) e -> Tick (Attributes ty (go a)) (go e)
    Tick t e -> Tick t (go e)
