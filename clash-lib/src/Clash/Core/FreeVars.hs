{-|
  Copyright   :  (C) 2012-2016, University of Twente
                     2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Free variable calculations
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.FreeVars
  (-- * Free variable calculation
    typeFreeVars
  , freeIds
  , freeLocalVars
  , freeLocalIds
  , globalIds
  , termFreeTyVars
  -- * Ordering type variables
  , dependencySortTyVars
  -- * occurrence check
  , globalIdOccursIn
  , localVarsDoNotOccurIn
  , countFreeOccurances
  -- * Internal
  , typeFreeVars'
  , termFreeVars'
  , termFreeIds'
  )
where

#if MIN_VERSION_ghc(9,8,4) || (MIN_VERSION_ghc(9,6,7) && !MIN_VERSION_ghc(9,8,0))
#define UNIQUE_IS_WORD64
#endif

import qualified Control.Lens           as Lens
import Control.Lens.Fold                (Fold)
import Control.Lens.Getter              (Contravariant)
import Data.Coerce
import qualified Data.List              as List
import Data.Maybe                       (mapMaybe)
#ifdef UNIQUE_IS_WORD64
import qualified GHC.Data.Word64Set     as IntSet
#else
import qualified Data.IntSet            as IntSet
#endif
import Data.Monoid                      (All (..), Any (..))

import Clash.Core.Term                  (Pat (..), Term (..), TickInfo (..), Bind(..))
import Clash.Core.Type                  (Type (..))
import Clash.Core.Var
  (Id, IdScope (..), TyVar, Var (..), isLocalId)
import Clash.Core.VarEnv
  (VarEnv, elemVarSet, emptyVarEnv, emptyVarSet, extendVarSet, lookupVarEnv,
   mkVarEnv, unionVarEnvWith, unitVarEnv)

-- | Gives the free type-variables in a Type, implemented as a 'Fold'
--
-- The 'Fold' is closed over the types of its variables, so:
--
-- @
-- foldMapOf typeFreeVars unitVarSet ((a:* -> k) Int) = {a, k}
-- @
typeFreeVars :: Fold Type TyVar
typeFreeVars = typeFreeVars' (const True) IntSet.empty

-- | Gives the "interesting" free variables in a Type, implemented as a 'Fold'
--
-- The 'Fold' is closed over the types of variables, so:
--
-- @
-- foldMapOf (typeFreeVars' (const True) IntSet.empty) unitVarSet ((a:* -> k) Int) = {a, k}
-- @
--
-- Note [Closing over kind variables]
--
-- Consider the type
--
-- > forall k . b -> k
--
-- where
--
-- > b :: k -> Type
--
-- When we close over the free variables of @forall k . b -> k@, i.e. @b@, then
-- the @k@ in @b :: k -> Type@ is most definitely /not/ the @k@ in
-- @forall k . b -> k@. So when a type variable is free, i.e. not in the inScope
-- set, its kind variables also aren´t; so in order to prevent collisions due to
-- shadowing we close using an empty inScope set.
--
-- See also: https://gitlab.haskell.org/ghc/ghc/-/commit/503514b94f8dc7bd9eab5392206649aee45f140b
typeFreeVars'
  :: (Contravariant f, Applicative f)
  => (forall b . Var b -> Bool)
  -- ^ Predicate telling whether a variable is interesting
#ifdef UNIQUE_IS_WORD64
  -> IntSet.Word64Set
#else
  -> IntSet.IntSet
#endif
  -- ^ Uniques of the type variables in scope, used by 'termFreeVars''
  -> (Var a -> f (Var a))
  -> Type
  -> f Type
typeFreeVars' interesting is f = go is where
  go inScope = \case
    VarTy tv -> tv1 <* go inScope1 (varType tv)
      where
        isInteresting = interesting tv
        tvInScope     = varUniq tv `IntSet.member` inScope
        inScope1
          | tvInScope = inScope
          | otherwise = IntSet.empty -- See Note [Closing over type variables]

        tv1 | isInteresting
            , not tvInScope
            = VarTy . coerce <$> f (coerce tv)
            | otherwise
            = pure (VarTy tv)

    ForAllTy tv ty -> ForAllTy <$> goBndr inScope tv
                               <*> go (IntSet.insert (varUniq tv) inScope) ty
    AppTy l r -> AppTy <$> go inScope l <*> go inScope r
    AnnType attrs ty -> AnnType attrs <$> go inScope ty
    ty -> pure ty

  goBndr inScope tv = (\t -> tv {varType = t}) <$> go inScope (varType tv)

-- | Check whether a set of variables does not occur free in a term
localVarsDoNotOccurIn
  :: [Var a]
  -> Term
  -> Bool
localVarsDoNotOccurIn vs e =
  getAll (Lens.foldMapOf freeLocalVars (All . (`notElem` vs)) e)

-- | Check whether a local identifier occurs free in a term
globalIdOccursIn
  :: Id
  -> Term
  -> Bool
globalIdOccursIn v e = getAny (Lens.foldMapOf globalIds (Any . (== v)) e)

-- | Calculate the /local/ free variable of an expression: the free type
-- variables and the free identifiers that are not bound in the global
-- environment.
freeLocalVars :: Fold Term (Var a)
freeLocalVars = termFreeVars' isLocalVar where
  isLocalVar (Id {idScope = GlobalId}) = False
  isLocalVar _ = True

-- | Gives the free identifiers of a Term, implemented as a 'Fold'
freeIds :: Fold Term Id
freeIds = termFreeIds' isId where
  isId (Id {}) = True
  isId _       = False

-- | Calculate the /local/ free identifiers of an expression: the free
-- identifiers that are not bound in the global environment.
freeLocalIds :: Fold Term Id
freeLocalIds = termFreeIds' isLocalId

-- | Calculate the /global/ free identifiers of an expression: the free
-- identifiers that are bound in the global environment.
globalIds :: Fold Term Id
globalIds = termFreeIds' isGlobalId where
  isGlobalId (Id {idScope = GlobalId}) = True
  isGlobalId _ = False

-- | Gives the free type-variables of a Term, implemented as a 'Fold'
--
-- The 'Fold' is closed over the types of variables, so:
--
-- @
-- foldMapOf termFreeTyVars unitVarSet (case (x : (a:* -> k) Int)) of {}) = {a, k}
-- @
termFreeTyVars :: Fold Term TyVar
termFreeTyVars = termFreeVars' isTV where
  isTV (TyVar {}) = True
  isTV _          = False

-- | Order type variables such that every type variable comes after the type
-- variables in its kind, so a term or type can abstract over them in that
-- order. A list that already has this property is returned unchanged.
--
-- Only the dependencies between the given type variables are taken into
-- account: a type variable in a kind that is not in the input list is ignored.
--
-- @
-- dependencySortTyVars [a :: k, k :: Type] = [k :: Type, a :: k]
-- @
dependencySortTyVars :: [TyVar] -> [TyVar]
dependencySortTyVars tvs = reverse (snd (List.foldl' visit (emptyVarSet, []) tvs))
 where
  tvEnv = mkVarEnv (zip tvs tvs)
  visit (seen, acc) tv
    | tv `elemVarSet` seen = (seen, acc)
    | otherwise =
        let kindTvs = mapMaybe (`lookupVarEnv` tvEnv)
                        (Lens.toListOf typeFreeVars (varType tv))
            (seen1, acc1) = List.foldl' visit (extendVarSet seen tv, acc) kindTvs
        in  (seen1, tv : acc1)

-- | Gives the "interesting" free variables in a Term, implemented as a 'Fold'
--
-- The 'Fold' is closed over the types of variables, so:
--
-- @
-- foldMapOf (termFreeVars' (const True)) unitVarSet (case (x : (a:* -> k) Int)) of {}) = {x, a, k}
-- @
--
-- Note [Closing over type variables]
--
-- Consider the term
--
-- > /\(k :: Type) -> \(b :: k) -> a
--
-- where
--
-- > a :: k
--
-- When we close over the free variables of @/\k -> \(b :: k) -> (a :: k)@, i.e.
-- @a@, then the @k@ in @a :: k@ is most definitely /not/ the @k@ in introduced
-- by the @/\k ->@. So when a term variable is free, i.e. not in the inScope
-- set, its type variables also aren´t; so in order to prevent collisions due to
-- shadowing we close using an empty inScope set.
--
-- See also: https://gitlab.haskell.org/ghc/ghc/-/commit/503514b94f8dc7bd9eab5392206649aee45f140b
termFreeVars'
  :: (Contravariant f, Applicative f)
  => (forall b . Var b -> Bool)
  -- ^ Predicate telling whether a variable is interesting
  -> (Var a -> f (Var a))
  -> Term
  -> f Term
termFreeVars' = termFreeVarsWorker True

-- | Gives the "interesting" free identifiers in a Term, implemented as a
-- 'Fold'. Like 'termFreeVars'', but fold does not descend into the types of
-- variables and binders.
termFreeIds'
  :: (Contravariant f, Applicative f)
  => (forall b . Var b -> Bool)
  -- ^ Predicate telling whether an identifier is interesting
  -> (Var a -> f (Var a))
  -> Term
  -> f Term
termFreeIds' = termFreeVarsWorker False

-- | Worker for 'termFreeVars'' and 'termFreeIds''. Has option that selects
-- whether to descend into types or not.
termFreeVarsWorker
  :: forall a f
   . (Contravariant f, Applicative f)
  => Bool
  -- ^ Descend into the types of variables and binders?
  -> (forall b . Var b -> Bool)
  -- ^ Predicate telling whether a variable is interesting
  -> (Var a -> f (Var a))
  -> Term
  -> f Term
termFreeVarsWorker descendIntoTypes interesting f = go IntSet.empty IntSet.empty where
  -- Type variables and term variables can share a unique, so we keep track of
  -- the type variables and the term variables in scope separately: a type
  -- binder doesn't bind a term variable with the same unique, or vice versa.

  -- Fold over a type only when descending into types; otherwise leave it as-is.
  onType tvScope ty
    | descendIntoTypes = typeFreeVars' interesting tvScope f ty
    | otherwise        = pure ty

  go tvScope tmScope = \case
    Var v
      | descendIntoTypes -> v1 <* typeFreeVars' interesting tvScope1 f (varType v)
      | otherwise        -> v1
      where
        isInteresting = interesting v
        vInScope      = isLocalId v && varUniq v `IntSet.member` tmScope
        tvScope1
          | vInScope  = tvScope
          | otherwise = IntSet.empty -- See Note [Closing over type variables]

        v1 | isInteresting
           , not vInScope
           = Var . coerce <$> f (coerce v)
           | otherwise
           = pure (Var v)

    Lam id_ tm ->
      Lam <$> goBndr tvScope id_
          <*> go tvScope (IntSet.insert (varUniq id_) tmScope) tm
    TyLam tv tm ->
      TyLam <$> goBndr tvScope tv
            <*> go (IntSet.insert (varUniq tv) tvScope) tmScope tm

    App l r ->
      App <$> go tvScope tmScope l <*> go tvScope tmScope r

    TyApp l r ->
      TyApp <$> go tvScope tmScope l
            <*> onType tvScope r

    Let (NonRec i x) e ->
      Let <$> (NonRec <$> goBndr tvScope i <*> go tvScope tmScope x)
          <*> go tvScope (IntSet.insert (varUniq i) tmScope) e

    Let (Rec bs) e ->
      Let <$> (Rec <$> traverse (goBind tvScope tmScope') bs)
          <*> go tvScope tmScope' e
     where
      tmScope' = foldr (IntSet.insert . varUniq . fst) tmScope bs

    Case subj ty alts ->
      Case <$> go tvScope tmScope subj
           <*> onType tvScope ty
           <*> traverse (goAlt tvScope tmScope) alts

    Cast tm t1 t2 ->
      Cast <$> go tvScope tmScope tm
           <*> onType tvScope t1
           <*> onType tvScope t2

    Tick tick tm ->
      Tick <$> goTick tvScope tmScope tick
      <*> go tvScope tmScope tm

    tm -> pure tm

  -- Fold over a binder's type only when descending; otherwise leave it as-is.
  goBndr tvScope v
    | descendIntoTypes = (\t -> v {varType = t}) <$> onType tvScope (varType v)
    | otherwise        = pure v

  goBind tvScope tmScope (l,r) =
    (,) <$> goBndr tvScope l <*> go tvScope tmScope r

  goAlt tvScope tmScope (pat,alt) = case pat of
    DataPat dc tvs ids -> (,) <$> (DataPat dc
                                     <$> traverse (goBndr tvScope') tvs
                                     <*> traverse (goBndr tvScope') ids)
                              <*> go tvScope' tmScope' alt
      where
        tvScope' = foldr (IntSet.insert . varUniq) tvScope tvs
        tmScope' = foldr (IntSet.insert . varUniq) tmScope ids
    _ -> (,) pat <$> go tvScope tmScope alt

  goTick tvScope tmScope = \case
    NameMod m ty -> NameMod m <$> onType tvScope ty
    Attributes ty tm -> Attributes <$> onType tvScope ty
                                   <*> go tvScope tmScope tm
    tick         -> pure tick
{-# INLINE termFreeVarsWorker #-}

-- | Get the free variables of an expression and count the number of occurrences
countFreeOccurances
  :: Term
  -> VarEnv Int
countFreeOccurances =
  Lens.foldMapByOf freeLocalIds (unionVarEnvWith (+)) emptyVarEnv
                   (`unitVarEnv` (1 :: Int))
