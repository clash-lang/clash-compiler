{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Free variable calculations
-}

{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Core.FreeVars
  (-- * Free variable calculation
    typeFreeVars
  , termFreeVars
  , termFreeIds
  , termFreeTyVars
  , tyFVsOfTypes
  , fVsOfTerms
  -- * Fast
  , noFreeVarsOfType
  -- * occurrence check
  , idOccursIn
  , idDoesNotOccurIn
  , idsDoNotOccurIn
  , varsDoNotOccurIn
  -- * Internal
  , typeFreeVars'
  , termFreeVars'
  )
where

import qualified Control.Lens           as Lens
import Control.Lens.Fold                (Fold)
import Control.Lens.Getter              (Contravariant)
import Data.Coerce
import qualified Data.IntSet            as IntSet
import Data.Monoid                      (All (..), Any (..))

import Clash.Core.Term                  (Pat (..), Term (..))
import Clash.Core.Type                  (Type (..))
import Clash.Core.Var                   (Id,TyVar,Var (..))
import Clash.Core.VarEnv                (VarSet, unitVarSet)

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
typeFreeVars'
  :: (Contravariant f, Applicative f)
  => (forall b . Var b -> Bool)
  -- ^ Predicate telling whether a variable is interesting
  -> IntSet.IntSet
  -- ^ Uniques of the variables in scope, used by 'termFreeVars''
  -> (Var a -> f (Var a))
  -> Type
  -> f Type
typeFreeVars' interesting is f = go is where
  go inScope = \case
    VarTy tv
      | interesting tv
      , varUniq tv `IntSet.notMember` inScope
      -> VarTy . coerce <$> f (coerce tv) <*
         go inScope (varType tv)
      | otherwise
      -> pure (VarTy tv)
    ForAllTy tv ty -> ForAllTy <$> goBndr inScope tv
                               <*> go (IntSet.insert (varUniq tv) inScope) ty
    AppTy l r -> AppTy <$> go inScope l <*> go inScope r
    ty -> pure ty

  goBndr inScope tv = (\t -> tv {varType = t}) <$> go inScope (varType tv)

-- | Check whether an identifier does not occur free in a term
idDoesNotOccurIn
  :: Id
  -> Term
  -> Bool
idDoesNotOccurIn v e = getAll (Lens.foldMapOf termFreeIds (All . (/= v)) e)

-- | Check whether a set of identifiers does not occur free in a term
idsDoNotOccurIn
  :: [Id]
  -> Term
  -> Bool
idsDoNotOccurIn vs e =
  getAll (Lens.foldMapOf termFreeIds (All . (`notElem` vs)) e)

-- | Check whether a set of variables does not occur free in a term
varsDoNotOccurIn
  :: [Var a]
  -> Term
  -> Bool
varsDoNotOccurIn vs e =
  getAll (Lens.foldMapOf termFreeVars (All . (`notElem` vs)) e)

-- | Check whether an identifier occurs free in a term
idOccursIn
  :: Id
  -> Term
  -> Bool
idOccursIn v e = getAny (Lens.foldMapOf termFreeIds (Any . (== v)) e)

-- | Gives the free identifiers of a Term, implemented as a 'Fold'
termFreeIds :: Fold Term Id
termFreeIds = termFreeVars' isId where
  isId (Id {}) = True
  isId _       = False

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

-- | Gives the free variables of a Term, implemented as a 'Fold'
--
-- The 'Fold' is closed over the types of variables, so:
--
-- @
-- foldMapOf termFreeVars unitVarSet (case (x : (a:* -> k) Int)) of {}) = {x, a, k}
-- @
termFreeVars :: Fold Term (Var a)
termFreeVars = termFreeVars' (const True)

-- | Gives the "interesting" free variables in a Term, implemented as a 'Fold'
--
-- The 'Fold' is closed over the types of variables, so:
--
-- @
-- foldMapOf (termFreeVars' (const True)) unitVarSet (case (x : (a:* -> k) Int)) of {}) = {x, a, k}
-- @
termFreeVars'
  :: (Contravariant f, Applicative f)
  => (forall b . Var b -> Bool)
  -- ^ Predicate telling whether a variable is interesting
  -> (Var a -> f (Var a))
  -> Term
  -> f Term
termFreeVars' interesting f = go IntSet.empty where
  go inScope = \case
    Var v
      | interesting v
      , varUniq v `IntSet.notMember` inScope
      -> Var . coerce <$> (f (coerce v)) <*
         typeFreeVars' interesting inScope f (varType v)
      | otherwise
      -> pure (Var v)
    Lam id_ tm -> Lam <$> goBndr inScope id_
                      <*> go (IntSet.insert (varUniq id_) inScope) tm
    TyLam tv tm -> TyLam <$> goBndr inScope tv
                         <*> go (IntSet.insert (varUniq tv) inScope) tm
    App l r -> App <$> go inScope l <*> go inScope r
    TyApp l r -> TyApp <$> go inScope l
                       <*> typeFreeVars' interesting inScope f r
    Letrec bs e -> Letrec <$> traverse (goBind inScope') bs <*> go inScope' e
      where inScope' = foldr IntSet.insert inScope (map (varUniq.fst) bs)
    Case subj ty alts -> Case <$> go inScope subj
                              <*> typeFreeVars' interesting inScope f ty
                              <*> traverse (goAlt inScope) alts
    Cast tm t1 t2 -> Cast <$> go inScope tm
                          <*> typeFreeVars' interesting inScope f t1
                          <*> typeFreeVars' interesting inScope f t2
    tm -> pure tm

  goBndr inScope v =
    (\t -> v  {varType = t}) <$> typeFreeVars' interesting inScope f (varType v)

  goBind inScope (l,r) = (,) <$> goBndr inScope l <*> go inScope r

  goAlt inScope (pat,alt) = case pat of
    DataPat dc tvs ids -> (,) <$> (DataPat <$> pure dc
                                           <*> traverse (goBndr inScope') tvs
                                           <*> traverse (goBndr inScope') ids)
                              <*> go inScope' alt
      where
        inScope' = foldr IntSet.insert
                         (foldr IntSet.insert inScope (map varUniq tvs))
                         (map varUniq ids)
    _ -> (,) <$> pure pat <*> go inScope alt

-- | Determine whether a type has no free type variables.
noFreeVarsOfType
  :: Type
  -> Bool
noFreeVarsOfType ty = case ty of
  VarTy {}    -> False
  ForAllTy {} -> getAll (Lens.foldMapOf typeFreeVars (const (All False)) ty)
  AppTy l r   -> noFreeVarsOfType l && noFreeVarsOfType r
  _           -> True

-- | Collect the free variables of a collection of type into a set
tyFVsOfTypes
  :: Foldable f
  => f Type
  -> VarSet
tyFVsOfTypes = foldMap go
 where
  go = Lens.foldMapOf typeFreeVars unitVarSet

-- | Collect the free variables of a collection of terms into a set
fVsOfTerms
  :: Foldable f
  => f Term
  -> VarSet
fVsOfTerms = foldMap go
 where
  go = Lens.foldMapOf termFreeVars unitVarSet
