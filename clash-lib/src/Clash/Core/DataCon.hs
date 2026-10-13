{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                          2017, Google Inc.,
                          2021, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Data Constructors in CoreHW

  Scoped Core refers to data constructors ('Data', 'DataPat') with
  'DataCon', which stores its closed type ('Clash.Core.DataCon.dcType') and
  its /signature/ (t'DataConSig', see "Clash.Core.Sig") as scoped
  types. Instantiating a data constructor substitutes types for the type
  variables of its signature, which renames binders where needed. The
  functions below take a t'TypeEnv', which they don't use: the signature is
  stored in the data constructor.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.DataCon
  ( DataCon (..)
  , DcName
  , ConTag
  , DcStrictness(..)
  , mkDataCon
  , dcUnivTyVarCount
  , dcExtTyVarCount
  , dcFieldCount
  , DataConSig (..)
  , dataConInstArgTys
  , dataConInstExtTyVars
  , withDataConPat
  , openDataConSig
  )
where

import Control.DeepSeq                        (NFData(..))
import Data.Binary                            (Binary (..))
import Data.Function                          (on)
import Data.Hashable                          (Hashable (hashWithSalt))
import qualified Data.Text                    as Text
import GHC.Generics                           (Generic)
import GHC.Stack                              (HasCallStack)

import Clash.Core.Name                        (Name (..))
import qualified Clash.Core.Name              as C
import Clash.Core.Sig                  (DataConSig (..))
import Clash.Core.Subst
import Clash.Core.Syntax hiding (Name)
import qualified Clash.Core.Syntax     as S
import Clash.Core.TypeEnv              (TypeEnv)
import Clash.Unique

-- | Data Constructor
data DataCon
  = MkData
  { dcName :: !DcName
  -- ^ Name of the DataCon
  , dcUniq :: {-# UNPACK #-} !Unique
  -- ^ Invariant: forall x . dcUniq x ~ nameUniq (dcName x)
  , dcTag :: !ConTag
  -- ^ Syntactical position in the type definition
  , dcType :: S.Type 'S.VoidS
  -- ^ Type of the 'DataCon', which is closed. Lazy.
  , dcSig :: DataConSig
  -- ^ Its universally and existentially quantified type variables, and the
  -- types of its fields, see "Clash.Core.Sig". The universally
  -- quantified type variables are also part of the result type of the
  -- 'DataCon', the existentially quantified ones are not. Lazy.
  , dcArgStrict :: [DcStrictness]
  -- ^ Argument strictness
  , dcFieldLabels :: [Text.Text]
  -- ^ Names of fields. Used when data constructor is referring to a record type.
  }

-- | A data constructor with the given name, tag, closed type, signature (see
-- "Clash.Core.Sig"), strictness of its fields, and labels of its
-- fields
mkDataCon
  :: DcName -> ConTag -> S.Type 'S.VoidS -> DataConSig -> [DcStrictness]
  -> [Text.Text] -> DataCon
mkDataCon nm tag ty sig strict labels = MkData
  { dcName = nm
  , dcUniq = nameUniq nm
  , dcTag = tag
  , dcType = ty
  , dcSig = sig
  , dcArgStrict = strict
  , dcFieldLabels = labels
  }

-- | The number of universally quantified type variables of a data constructor
dcUnivTyVarCount :: DataCon -> Int
dcUnivTyVarCount dc = case dcSig dc of
  DataConSig univ _ _ -> S.teleLength univ

-- | The number of existentially quantified type variables of a data
-- constructor
dcExtTyVarCount :: DataCon -> Int
dcExtTyVarCount dc = case dcSig dc of
  DataConSig _ ext _ -> S.teleLength ext

-- | The number of fields of a data constructor
dcFieldCount :: DataCon -> Int
dcFieldCount dc = case dcSig dc of
  DataConSig _ _ fields -> length fields

instance Show DataCon where
  show = show . dcName

-- | Doesn't force the type and the signature, which are lazy
instance NFData DataCon where
  rnf dc =
    rnf (dcName dc) `seq` rnf (dcTag dc) `seq` rnf (dcArgStrict dc)
      `seq` rnf (dcFieldLabels dc)

instance Eq DataCon where
  (==) = (==) `on` dcUniq
  (/=) = (/=) `on` dcUniq

instance Ord DataCon where
  compare = compare `on` dcUniq

instance Hashable DataCon where
  hashWithSalt salt = hashWithSalt salt . dcUniq

instance Uniquable DataCon where
  getUnique = dcUniq
  setUnique dc u = dc {dcUniq=u, dcName=(dcName dc){nameUniq=u}}

-- | Syntactical position of the DataCon in the type definition
type ConTag = Int
-- | DataCon reference
type DcName = Name DataCon

data DcStrictness
  = Strict
  | Lazy
  deriving (Generic, NFData, Eq, Hashable, Binary)

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
  -> [C.Name Term]
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
