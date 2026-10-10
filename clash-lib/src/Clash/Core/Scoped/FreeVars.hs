{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Free variables of scoped Clash Core.

  Type variables and term variables are collected separately: a type never
  mentions a term variable, so the free term variables of a term are found
  without looking at its types.

  Free variables are /closed over types/: the free variables of a term include
  those of the types of its variable occurrences and binders, and the free
  variables of a type include those of the kinds of its type variables. E.g.
  the type @forall k. b@, where the free type variable @b@ has kind @k@, has
  free variables @b@ and the @k@ of the kind, which is not the @k@ that the
  @forall@ binds.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.Scoped.FreeVars
  ( -- * Free variables
    freeVars
  , freeTmVars
  , freeTyVars
  , freeTyVarsOfType
  , termGlobalIds
    -- * Occurrences
  , tmNameOccursIn
  , tyNameOccursIn
  , tyNameOccursInType
  , anyBoundOccursIn
  , anyBoundOccursInType
  , patBindersOccurIn
    -- * Names bound by binders
  , patNameBinders
  , bindNameBinders
  , teleNameBinders
  , HasNameBinder (..)
  ) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List

import Clash.Core.Scoped.Internal.Names
import Clash.Core.Scoped.Internal.Scope (nameId, nameOf)
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Syntax

-- * Names bound by binders

-- | Binders of a single local variable
class HasNameBinder b where
  -- | The (unsorted) binder of the variable
  nameBinderOf :: b n l -> NameBinder n l

instance HasNameBinder TyBinder where
  nameBinderOf = unsortTyNameBinder . tyBinderName
  {-# INLINE nameBinderOf #-}

instance HasNameBinder IdBinder where
  nameBinderOf = unsortTmNameBinder . idBinderName
  {-# INLINE nameBinderOf #-}

-- | The raw name a binder binds
binderRaw :: HasNameBinder b => b n l -> Int
binderRaw = nameId . nameOf . nameBinderOf
{-# INLINE binderRaw #-}

-- | The names a telescope binds
teleNameBinders :: HasNameBinder b => Tele b n l -> NameBinders n l
teleNameBinders = go emptyNameBinders
 where
  go :: HasNameBinder b => NameBinders n i -> Tele b i l -> NameBinders n l
  go !acc TNil = acc
  go !acc (TCons b bs) = go (snocNameBinders acc (nameBinderOf b)) bs

-- | The names a pattern binds: the existential type variables and the fields
-- of a 'DataPat'
patNameBinders :: Pat n l -> NameBinders n l
patNameBinders = \case
  DataPat _ tvs ids -> goIds (goTvs emptyNameBinders tvs) ids
  LitPat _ -> emptyNameBinders
  DefaultPat -> emptyNameBinders
 where
  -- Not 'teleNameBinders', to build a single set for both telescopes
  goTvs :: NameBinders n i -> Tele TyBinder i l -> NameBinders n l
  goTvs !acc TNil = acc
  goTvs !acc (TCons b bs) = goTvs (snocNameBinders acc (nameBinderOf b)) bs

  goIds :: NameBinders n i -> Tele IdBinder i l -> NameBinders n l
  goIds !acc TNil = acc
  goIds !acc (TCons b bs) = goIds (snocNameBinders acc (nameBinderOf b)) bs

-- | The names let-bindings bind
bindNameBinders :: Bind n l -> NameBinders n l
bindNameBinders = \case
  NonRec b _ -> snocNameBinders emptyNameBinders (nameBinderOf b)
  Rec bs _ -> teleNameBinders bs

-- Move a set out of the scope of a telescope
unsinkTele :: HasNameBinder b => Tele b n l -> NameSet k l -> NameSet k n
unsinkTele TNil s = s
unsinkTele (TCons b bs) s = unsinkNameSetBinder (nameBinderOf b) (unsinkTele bs s)

-- Move a set out of the scope of a pattern
unsinkPat :: Pat n l -> NameSet k l -> NameSet k n
unsinkPat p s = case p of
  DataPat _ tvs ids -> unsinkTele tvs (unsinkTele ids s)
  LitPat _ -> s
  DefaultPat -> s

-- * Free variables

-- | The free type variables and the free term variables of a term. See
-- 'freeTyVars' and 'freeTmVars'.
freeVars :: Term n -> (NameSet TyName n, NameSet TmName n)
freeVars e = (freeTyVars e, freeTmVars e)

-- | The free term variables of a term
freeTmVars :: Term n -> NameSet TmName n
freeTmVars = \case
  Var _ _ n -> singletonNameSet n
  GlobalVar {} -> emptyNameSet
  Data {} -> emptyNameSet
  Literal {} -> emptyNameSet
  Prim {} -> emptyNameSet
  Lam b e -> unsinkNameSetBinder (nameBinderOf b) (freeTmVars e)
  TyLam b e -> unsinkNameSetBinder (nameBinderOf b) (freeTmVars e)
  App e1 e2 -> freeTmVars e1 <> freeTmVars e2
  TyApp e _ -> freeTmVars e
  Let (NonRec b x) e -> freeTmVars x <> unsinkNameSetBinder (nameBinderOf b) (freeTmVars e)
  Let (Rec bs xs) e -> unsinkTele bs (unionsNameSet (map freeTmVars (e:xs)))
  Case e _ alts -> unionsNameSet (freeTmVars e : map alt alts)
  Cast e _ _ -> freeTmVars e
  Tick t e -> tick t <> freeTmVars e
 where
  alt :: Alt n -> NameSet TmName n
  alt (Alt p e) = unsinkPat p (freeTmVars e)

  tick :: TickInfo n -> NameSet TmName n
  tick = \case
    Attributes _ e -> freeTmVars e
    _ -> emptyNameSet

-- | The free type variables of a term, including those of the types of its
-- variable occurrences and binders
freeTyVars :: Term n -> NameSet TyName n
freeTyVars = \case
  Var _ ty _ -> freeTyVarsOfType ty
  GlobalVar {} -> emptyNameSet
  Data {} -> emptyNameSet
  Literal {} -> emptyNameSet
  Prim {} -> emptyNameSet
  Lam b e ->
    freeTyVarsOfType (idBinderType b) <> unsinkNameSetBinder (nameBinderOf b) (freeTyVars e)
  TyLam b e ->
    freeTyVarsOfType (tyBinderKind b) <> unsinkNameSetBinder (nameBinderOf b) (freeTyVars e)
  App e1 e2 -> freeTyVars e1 <> freeTyVars e2
  TyApp e ty -> freeTyVars e <> freeTyVarsOfType ty
  Let (NonRec b x) e ->
    freeTyVarsOfType (idBinderType b) <> freeTyVars x
      <> unsinkNameSetBinder (nameBinderOf b) (freeTyVars e)
  Let (Rec bs xs) e -> idTele bs (unionsNameSet (map freeTyVars (e:xs)))
  Case e ty alts -> unionsNameSet (freeTyVars e : freeTyVarsOfType ty : map alt alts)
  Cast e t1 t2 -> freeTyVars e <> freeTyVarsOfType t1 <> freeTyVarsOfType t2
  Tick t e -> tick t <> freeTyVars e
 where
  alt :: Alt n -> NameSet TyName n
  alt (Alt p e) = case p of
    DataPat _ tvs ids -> tyTele tvs (idTele ids (freeTyVars e))
    LitPat _ -> freeTyVars e
    DefaultPat -> freeTyVars e

  tick :: TickInfo n -> NameSet TyName n
  tick = \case
    NameMod _ ty -> freeTyVarsOfType ty
    Attributes ty e -> freeTyVarsOfType ty <> freeTyVars e
    _ -> emptyNameSet

  -- The free type variables of the types of the binders of a telescope, and
  -- of the given ones of its scope
  idTele :: Tele IdBinder n l -> NameSet TyName l -> NameSet TyName n
  idTele TNil inner = inner
  idTele (TCons b bs) inner =
    freeTyVarsOfType (idBinderType b)
      <> unsinkNameSetBinder (nameBinderOf b) (idTele bs inner)

  tyTele :: Tele TyBinder n l -> NameSet TyName l -> NameSet TyName n
  tyTele TNil inner = inner
  tyTele (TCons b bs) inner =
    freeTyVarsOfType (tyBinderKind b)
      <> unsinkNameSetBinder (nameBinderOf b) (tyTele bs inner)

-- | The free type variables of a type, including those of the kinds of its
-- type variables
freeTyVarsOfType :: Type n -> NameSet TyName n
freeTyVarsOfType = \case
  VarTy _ k n -> insertNameSet n (freeTyVarsOfType k)
  ConstTy {} -> emptyNameSet
  ForAllTy b t ->
    freeTyVarsOfType (tyBinderKind b)
      <> unsinkNameSetBinder (nameBinderOf b) (freeTyVarsOfType t)
  AppTy t1 t2 -> freeTyVarsOfType t1 <> freeTyVarsOfType t2
  LitTy {} -> emptyNameSet
  AnnType _ t -> freeTyVarsOfType t

-- | The global variables a term refers to
termGlobalIds :: Term n -> GlobalSet
termGlobalIds = go emptyGlobalSet
 where
  go :: GlobalSet -> Term x -> GlobalSet
  go !acc = \case
    Var {} -> acc
    GlobalVar g -> insertGlobalSet g acc
    Data {} -> acc
    Literal {} -> acc
    Prim {} -> acc
    Lam _ e -> go acc e
    TyLam _ e -> go acc e
    App e1 e2 -> go (go acc e1) e2
    TyApp e _ -> go acc e
    Let (NonRec _ x) e -> go (go acc x) e
    Let (Rec _ xs) e -> List.foldl' go (go acc e) xs
    Case e _ alts -> List.foldl' (\a (Alt _ e') -> go a e') (go acc e) alts
    Cast e _ _ -> go acc e
    Tick (Attributes _ e') e -> go (go acc e') e
    Tick _ e -> go acc e

-- * Occurrences

-- | Whether a term variable occurs free in a term
tmNameOccursIn :: TmName n -> Term n -> Bool
tmNameOccursIn n = anyFreeIn False (IntSet.singleton (tmNameRaw n))

-- | Whether a type variable occurs free in a term, e.g. in the type of a
-- variable occurrence
tyNameOccursIn :: TyName n -> Term n -> Bool
tyNameOccursIn n = anyFreeIn True (IntSet.singleton (tyNameRaw n))

-- | Whether a type variable occurs free in a type, e.g. in the kind of a type
-- variable
tyNameOccursInType :: TyName n -> Type n -> Bool
tyNameOccursInType n = anyFreeInTy (IntSet.singleton (tyNameRaw n))

-- | Whether any of the names bound between scopes @n@ and @l@ occurs free in
-- a term in scope @l@
anyBoundOccursIn :: NameBinders n l -> Term l -> Bool
anyBoundOccursIn bs = anyFreeIn True (nameBindersRaw bs)

-- | Whether any of the names bound between scopes @n@ and @l@ occurs free in
-- a type in scope @l@
anyBoundOccursInType :: NameBinders n l -> Type l -> Bool
anyBoundOccursInType bs = anyFreeInTy (nameBindersRaw bs)

-- | Whether any of the variables a pattern binds occurs free in a term. Only
-- looks at types if the pattern binds type variables.
patBindersOccurIn :: Pat n l -> Term l -> Bool
patBindersOccurIn p = case p of
  DataPat _ tvs _ -> anyFreeIn (not (isTNil tvs)) (nameBindersRaw (patNameBinders p))
  LitPat _ -> const False
  DefaultPat -> const False
 where
  isTNil :: Tele b x y -> Bool
  isTNil TNil = True
  isTNil _ = False

-- | Whether any of the given raw names occurs free in a type. Accounts for
-- shadowing: a binder with one of the names hides it in its scope.
anyFreeInTy :: IntSet -> Type n -> Bool
anyFreeInTy ns0 ty0 = not (IntSet.null ns0) && go ns0 ty0
 where
  go :: IntSet -> Type x -> Bool
  go ns = \case
    VarTy _ k n -> IntSet.member (tyNameRaw n) ns || go ns k
    ConstTy {} -> False
    ForAllTy b t -> go ns (tyBinderKind b) || under ns (binderRaw b) t
    AppTy t1 t2 -> go ns t1 || go ns t2
    LitTy {} -> False
    AnnType _ t -> go ns t

  under :: IntSet -> Int -> Type x -> Bool
  under ns k t = let ns' = IntSet.delete k ns in not (IntSet.null ns') && go ns' t

-- | Whether any of the given raw names occurs free in a term. Accounts for
-- shadowing: a binder with one of the names hides it in its scope. Only looks
-- at types if the first argument is 'True': if the names may include type
-- variables.
anyFreeIn :: Bool -> IntSet -> Term n -> Bool
anyFreeIn tys ns0 tm0 = not (IntSet.null ns0) && goTm ns0 tm0
 where
  goTy :: IntSet -> Type x -> Bool
  goTy ns ty = tys && anyFreeInTy ns ty
  {-# INLINE goTy #-}

  under :: IntSet -> Int -> (IntSet -> a -> Bool) -> a -> Bool
  under ns k f x =
    let ns' = IntSet.delete k ns in not (IntSet.null ns') && f ns' x
  {-# INLINE under #-}

  goTm :: IntSet -> Term x -> Bool
  goTm ns = \case
    Var _ ty n -> IntSet.member (tmNameRaw n) ns || goTy ns ty
    GlobalVar {} -> False
    Data {} -> False
    Literal {} -> False
    Prim {} -> False
    Lam b e -> goTy ns (idBinderType b) || under ns (binderRaw b) goTm e
    TyLam b e -> goTy ns (tyBinderKind b) || under ns (binderRaw b) goTm e
    App e1 e2 -> goTm ns e1 || goTm ns e2
    TyApp e ty -> goTm ns e || goTy ns ty
    Let (NonRec b x) e ->
      goTy ns (idBinderType b) || goTm ns x || under ns (binderRaw b) goTm e
    Let (Rec bs xs) e -> goIdTele ns bs (\ns' -> any (goTm ns') (e:xs))
    Case e ty alts -> goTm ns e || goTy ns ty || any (goAlt ns) alts
    Cast e t1 t2 -> goTm ns e || goTy ns t1 || goTy ns t2
    Tick t e -> goTick ns t || goTm ns e

  goAlt :: IntSet -> Alt x -> Bool
  goAlt ns (Alt p e) = case p of
    DataPat _ tvs ids -> goTyTele ns tvs (\ns1 -> goIdTele ns1 ids (\ns2 -> goTm ns2 e))
    _ -> goTm ns e

  goTick :: IntSet -> TickInfo x -> Bool
  goTick ns = \case
    NameMod _ ty -> goTy ns ty
    Attributes ty e -> goTy ns ty || goTm ns e
    _ -> False

  goIdTele :: IntSet -> Tele IdBinder x y -> (IntSet -> Bool) -> Bool
  goIdTele ns TNil k = k ns
  goIdTele ns (TCons b bs) k =
    goTy ns (idBinderType b) || under ns (binderRaw b) (\ns' bs' -> goIdTele ns' bs' k) bs

  goTyTele :: IntSet -> Tele TyBinder x y -> (IntSet -> Bool) -> Bool
  goTyTele ns TNil k = k ns
  goTyTele ns (TCons b bs) k =
    goTy ns (tyBinderKind b) || under ns (binderRaw b) (\ns' bs' -> goTyTele ns' bs' k) bs
