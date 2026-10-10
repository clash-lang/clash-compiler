{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Data constructors in scoped Clash Core, and their instantiation.

  = Transition

  Scoped Core refers to data constructors ('Data', 'DataPat') with Clash's
  'DataCon', whose types are closed Clash 'C.Type's. Until 'DataCon' and
  'Clash.Core.TyCon.TyCon' store scoped types, this module gives the
  /signature/ of a data constructor as scoped types (t'DataConSig'), by
  converting its Clash types ('dataConSig'): a telescope of its universally
  quantified type variables, in the empty scope; a telescope of its
  existentially quantified type variables, in the scope of the universal ones;
  and the types of its fields, in the scope of both. Instantiating a data
  constructor is then substituting types for the variables of these
  telescopes, which renames binders where needed. Contrast this with
  'Clash.Core.Util.dataConInstArgTys', which may capture free variables of the
  types it substitutes.

  The conversion is redone for every instantiation. It is cheap, as the types
  of data constructors are small; if it shows up in profiles, cache the
  signatures by 'dcUniq'.

  = Migration

  Once the 'DataCon' of scoped Core stores a t'DataConSig' (in place of
  'dcType', 'dcUnivTyVars', 'dcExtTyVars' and 'dcArgTys'), 'dataConSig'
  becomes a field selector, and the functions below keep working as they are.
  Their users don't depend on how the signature is obtained.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.Scoped.DataCon
  ( DataConSig (..)
  , dataConSig
  , dataConType
  , dataConInstArgTys
  , dataConInstExtTyVars
  , withDataConPat
  ) where

import GHC.Stack (HasCallStack)

import Clash.Core.DataCon (DataCon (..))
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax

-- | The signature of a data constructor, see the module header
data DataConSig where
  DataConSig
    :: (DExt 'VoidS u, DExt u e)
    => Tele TyBinder 'VoidS u
    -- ^ Universally quantified type variables
    -> Tele TyBinder u e
    -- ^ Existentially quantified type variables
    -> [Type e]
    -- ^ Types of the fields
    -> DataConSig

deriving instance Show DataConSig

-- | The signature of a data constructor. Errors if the types of its fields
-- mention type variables other than its universally and existentially
-- quantified ones.
dataConSig :: HasCallStack => DataCon -> DataConSig
dataConSig dc =
  fromClashTyVars emptyConvEnv (dcUnivTyVars dc) $ \env1 univ ->
    fromClashTyVars env1 (dcExtTyVars dc) $ \env2 ext ->
      DataConSig univ ext (map (fromClashType env2) (dcArgTys dc))

-- | The type of a data constructor, in any scope
dataConType :: (HasCallStack, Distinct n) => Scope n -> DataCon -> Type n
dataConType sc dc = fromClashTypeClosed sc (dcType dc)

-- | The types of the fields of a data constructor, given types for its
-- universally and its existentially quantified type variables. 'Nothing' if
-- there are more or fewer types than type variables.
dataConInstArgTys
  :: (HasCallStack, Distinct n) => Scope n -> DataCon -> [Type n] -> Maybe [Type n]
dataConInstArgTys sc dc tys = case dataConSig dc of
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
  => Scope n
  -> DataCon
  -> [Type n]
  -- ^ Types for the universally quantified type variables
  -> (forall l. DExt n l => Scope l -> Tele TyBinder n l -> [Type l] -> r)
  -> Maybe r
dataConInstExtTyVars sc dc univTys cont = case dataConSig dc of
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
  => Scope n
  -> DataCon
  -> [Type n]
  -- ^ Types for the universally quantified type variables
  -> [C.Name C.Term]
  -- ^ Human readable names for the fields. Their uniques are not used.
  -> (forall l. DExt n l => Scope l -> Pat n l -> [Term l] -> r)
  -> Maybe r
withDataConPat sc dc univTys names cont = do
  res <- dataConInstExtTyVars sc dc univTys $ \sc1 ext argTys ->
    if length names /= length argTys then
      Nothing
    else
      Just $ withFreshIdBinders sc1 (zip names argTys) $ \sc2 fields vars ->
        cont sc2 (DataPat dc ext fields) vars
  res
