{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Data constructors in scoped Clash Core, and their instantiation.

  Scoped Core refers to data constructors ('Data', 'DataPat') with
  'DataCon', which stores its closed type ('Clash.Core.DataCon.dcType') and
  its /signature/ (t'DataConSig', see "Clash.Core.Scoped.Sig") as scoped
  types. Instantiating a data constructor substitutes types for the type
  variables of its signature, which renames binders where needed. The
  functions below take a t'TypeEnv', which they don't need any more: the
  signature is stored in the data constructor. Contrast
  this with 'Clash.Core.Util.dataConInstArgTys', which may capture free
  variables of the types it substitutes.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.Scoped.DataCon
  ( DataConSig (..)
  , dataConInstArgTys
  , dataConInstExtTyVars
  , withDataConPat
  , openDataConSig
  ) where

import GHC.Stack (HasCallStack)

import Clash.Core.DataCon (DataCon (dcSig))
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C

import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Sig (DataConSig (..))
import Clash.Core.Scoped.TypeEnv (TypeEnv)

-- | The types of the fields of a data constructor, given types for its
-- universally and its existentially quantified type variables. 'Nothing' if
-- there are more or fewer types than type variables.
dataConInstArgTys
  :: (HasCallStack, Distinct n)
  => TypeEnv -> Scope n -> DataCon -> [Type n] -> Maybe [Type n]
dataConInstArgTys _ sc dc tys = case dcSig dc of
  DataConSig univ ext argTys -> do
    let (univTys, extTys) = splitAt (teleLength univ) tys
    s1 <- extendTySubstTele (closedSubst sc) univ univTys
    s2 <- extendTySubstTele s1 ext extTys
    pure (map (substTy s2) argTys)

-- | Instantiate the universally quantified type variables of a data
-- constructor with the given types, and bind its existentially quantified type
-- variables. Gives the binders of the existentially quantified type variables,
-- and the types of the fields in their scope. 'Nothing' if there are more or
-- fewer types than universally quantified type variables.
dataConInstExtTyVars
  :: (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> DataCon
  -> [Type n]
  -- ^ Types for the universally quantified type variables
  -> (forall l. DExt n l => Scope l -> Tele TyBinder n l -> [Type l] -> r)
  -> Maybe r
dataConInstExtTyVars _ sc dc univTys cont = case dcSig dc of
  DataConSig univ ext argTys -> do
    s1 <- extendTySubstTele (closedSubst sc) univ univTys
    pure $ substTele substTyBinder s1 ext $ \s2 ext' ->
      cont (substScope s2) ext' (map (substTy s2) argTys)

-- | A pattern that matches a data constructor, with fresh binders for its
-- existentially quantified type variables and its fields. Gives the pattern
-- and the variables of its fields. 'Nothing' if there are more or fewer types
-- than universally quantified type variables, or more or fewer names than
-- fields.
withDataConPat
  :: (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> DataCon
  -> [Type n]
  -- ^ Types for the universally quantified type variables
  -> [C.Name C.Term]
  -- ^ Human readable names for the fields. Their uniques are not used.
  -> (forall l. DExt n l => Scope l -> Pat n l -> [Term l] -> r)
  -> Maybe r
withDataConPat env sc dc univTys names cont = do
  res <- dataConInstExtTyVars env sc dc univTys $ \sc1 ext argTys ->
    if length names /= length argTys then
      Nothing
    else
      Just $ withFreshIdBinders sc1 (zip names argTys) $ \sc2 fields vars ->
        cont sc2 (DataPat dc ext fields) vars
  res

-- | The signature of a data constructor, with fresh binders: its universally
-- and existentially quantified type variables, and the types of its fields in
-- their scope
openDataConSig
  :: DataCon
  -> (forall u e. (DExt 'VoidS u, DExt u e)
        => Scope e -> Tele TyBinder 'VoidS u -> Tele TyBinder u e -> [Type e] -> r)
  -> r
openDataConSig dc k = case dcSig dc of
  DataConSig univ ext argTys ->
    substTele substTyBinder (closedSubst emptyScope) univ $ \s1 univ' ->
      substTele substTyBinder s1 ext $ \s2 ext' ->
        k (substScope s2) univ' ext' (map (substTy s2) argTys)
