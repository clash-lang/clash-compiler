{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The signatures of type constructors and data constructors in scoped Clash
  Core ("Clash.Core.Scoped.Sig"), converted from Clash Core.

  = Transition

  Scoped types refer to type constructors by name (in a 'C.ConstTy'), and
  look them up in Clash's 'Clash.Core.TyCon.TyConMap', whose 'TyCon's have
  closed Clash 'C.Type's. Scoped terms refer to data constructors with Clash's
  'DataCon', whose types are closed Clash types too. This module gives the
  parts of a 'TyCon' and a 'DataCon' that mention type variables as scoped
  types, by converting them (TODO(D4): read them once 'TyCon' and 'DataCon'
  store scoped types).

  Clash stores the equations of a type family without their pattern
  variables, so these are the free type variables of the equation, ordered so
  that every variable comes after the variables in its kind.

  The functions below convert for every call. Use the ones of
  "Clash.Core.Scoped.TypeEnv", which convert every signature at most once.

  = Migration

  Once 'TyCon' and 'DataCon' store scoped types (in place of 'ntEtadRhs',
  'tyConSubst', 'dcUnivTyVars', 'dcExtTyVars' and 'dcArgTys'), the functions
  below become field selectors.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.Scoped.TyCon
  ( -- * Newtypes
    NewTyConSig (..)
  , newTyConSig
  , newTyConInstRhs
    -- * Type families
  , TyFamEqn (..)
  , tyFamEqns
    -- * Data constructors
  , DataConSig (..)
  , dataConSig
  ) where

import qualified Control.Lens as Lens
import qualified Data.List as List
import GHC.Stack (HasCallStack)

import Clash.Core.DataCon (DataCon (..))
import Clash.Core.FreeVars (dependencySortTyVars, typeFreeVars)
import Clash.Core.TyCon (AlgTyConRhs (..), TyCon (..))
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Sig
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax

-- * Newtypes

-- | The signature of a newtype. 'Nothing' if the type constructor is not a
-- newtype. Errors if the representation type mentions type variables other
-- than those of the newtype.
newTyConSig :: HasCallStack => TyCon -> Maybe NewTyConSig
newTyConSig = \case
  AlgTyCon {algTcRhs = NewTyCon {ntEtadRhs = (tvs, rhs)}} ->
    Just $ fromClashTyVars emptyConvEnv tvs $ \env tele ->
      NewTyConSig tele (fromClashType env rhs)
  _ -> Nothing

-- | The representation type of a newtype applied to the given types: the types
-- are substituted for the type variables of the newtype, and the remaining
-- ones are applied to the result. 'Nothing' if there are fewer types than
-- type variables.
--
-- See 'Clash.Core.Type.coreView1'.
newTyConInstRhs :: Distinct n => Scope n -> NewTyConSig -> [Type n] -> Maybe (Type n)
newTyConInstRhs sc (NewTyConSig tvs rhs) = instTele (closedSubst sc) tvs rhs

-- Substitute the types for the variables of the telescope, and apply the
-- remaining ones
instTele
  :: Distinct n => Subst i n -> Tele TyBinder i p -> Type p -> [Type n] -> Maybe (Type n)
instTele s TNil rhs tys = Just (List.foldl' AppTy (substTy s rhs) tys)
instTele s (TCons b bs) rhs (ty:tys) = instTele (extendTySubst s b ty) bs rhs tys
instTele _ TCons {} _ [] = Nothing

-- * Type families

-- | The equations of a type family, in order. 'Nothing' if the type
-- constructor is not a type family.
tyFamEqns :: HasCallStack => TyCon -> Maybe [TyFamEqn]
tyFamEqns = \case
  FunTyCon {tyConSubst = eqns} -> Just (map tyFamEqn eqns)
  _ -> Nothing

tyFamEqn :: HasCallStack => ([C.Type], C.Type) -> TyFamEqn
tyFamEqn (lhs, rhs) =
  fromClashTyVars emptyConvEnv tvs $ \env tele ->
    TyFamEqn tele (map (fromClashType env) lhs) (fromClashType env rhs)
 where
  -- The free type variables, closed over kinds, see the module header
  tvs =
    dependencySortTyVars
      (List.nubBy sameUniq (concatMap (Lens.toListOf typeFreeVars) (lhs ++ [rhs])))

  sameUniq :: C.TyVar -> C.TyVar -> Bool
  sameUniq v1 v2 = C.varUniq v1 == C.varUniq v2

-- * Data constructors

-- | The signature of a data constructor. Errors if the types of its fields
-- mention type variables other than its universally and existentially
-- quantified ones.
dataConSig :: HasCallStack => DataCon -> DataConSig
dataConSig dc =
  fromClashTyVars emptyConvEnv (dcUnivTyVars dc) $ \env1 univ ->
    fromClashTyVars env1 (dcExtTyVars dc) $ \env2 ext ->
      DataConSig univ ext (map (fromClashType env2) (dcArgTys dc))
