{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                          2017, Google Inc.,
                          2021, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Data Constructors in CoreHW
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Clash.Core.DataCon
  ( DataCon (..)
  , DcName
  , ConTag
  , DcStrictness(..)
    -- * Transition
  , LegacyDataCon (..)
  , dcLegacyType
  , dcUnivTyVars
  , dcExtTyVars
  , dcArgTys
  , dcUnivTyVarCount
  , dcExtTyVarCount
  , dcFieldCount
  )
where

import Control.DeepSeq                        (NFData(..))
import Data.Binary                            (Binary (..))
import Data.Function                          (on)
import Data.Hashable                          (Hashable (hashWithSalt))
import qualified Data.Text                    as Text
import GHC.Generics                           (Generic)

import Clash.Core.Name                        (Name (..))
import Clash.Core.Scoped.Sig                  (DataConSig (..))
import qualified Clash.Core.Scoped.Syntax     as Scoped
import {-# SOURCE #-} Clash.Core.Type         (Type)
import Clash.Core.Var                         (TyVar)
import Clash.Unique

-- | Data Constructor
--
-- = Transition
--
-- The scoped fields, 'dcType' and 'dcSig', are the truth. 'dcLegacy' holds
-- the types of the data constructor as Clash Core's types, for the code that
-- still uses Clash Core. It is derived lazily, and is deleted with Clash Core
-- (package B of the port). Make data constructors with
-- 'Clash.Core.Scoped.Convert.mkDataCon' or
-- 'Clash.Core.Scoped.Convert.mkDataConFromClash', which derive the side they
-- are not given.
data DataCon
  = MkData
  { dcName :: !DcName
  -- ^ Name of the DataCon
  , dcUniq :: {-# UNPACK #-} !Unique
  -- ^ Invariant: forall x . dcUniq x ~ nameUniq (dcName x)
  , dcTag :: !ConTag
  -- ^ Syntactical position in the type definition
  , dcType :: Scoped.Type 'Scoped.VoidS
  -- ^ Type of the 'DataCon', which is closed. Lazy.
  , dcSig :: DataConSig
  -- ^ Its universally and existentially quantified type variables, and the
  -- types of its fields, see "Clash.Core.Scoped.Sig". The universally
  -- quantified type variables are also part of the result type of the
  -- 'DataCon', the existentially quantified ones are not. Lazy.
  , dcArgStrict :: [DcStrictness]
  -- ^ Argument strictness
  , dcFieldLabels :: [Text.Text]
  -- ^ Names of fields. Used when data constructor is referring to a record type.
  , dcLegacy :: LegacyDataCon
  -- ^ Transitional, see above. Lazy.
  }

-- | The types of a data constructor as Clash Core's types. Transitional, see
-- 'DataCon'.
data LegacyDataCon
  = LegacyDataCon
  { ldcType :: !Type
  -- ^ Type of the 'DataCon'
  , ldcUnivTyVars :: [TyVar]
  -- ^ Universally quantified type-variables, these type variables are also part
  -- of the result type of the DataCon
  , ldcExtTyVars :: [TyVar]
  -- ^ Existentially quantified type-variables, these type variables are not
  -- part of the result of the DataCon, but only of the arguments.
  , ldcArgTys :: [Type]
  -- ^ Argument types
  }

-- | The type of a data constructor, as a type of Clash Core. Transitional, see
-- 'DataCon'.
dcLegacyType :: DataCon -> Type
dcLegacyType = ldcType . dcLegacy

-- | Transitional, see 'DataCon' and 'ldcUnivTyVars'
dcUnivTyVars :: DataCon -> [TyVar]
dcUnivTyVars = ldcUnivTyVars . dcLegacy

-- | Transitional, see 'DataCon' and 'ldcExtTyVars'
dcExtTyVars :: DataCon -> [TyVar]
dcExtTyVars = ldcExtTyVars . dcLegacy

-- | Transitional, see 'DataCon' and 'ldcArgTys'
dcArgTys :: DataCon -> [Type]
dcArgTys = ldcArgTys . dcLegacy

-- | The number of universally quantified type variables of a data constructor
dcUnivTyVarCount :: DataCon -> Int
dcUnivTyVarCount dc = case dcSig dc of
  DataConSig univ _ _ -> Scoped.teleLength univ

-- | The number of existentially quantified type variables of a data
-- constructor
dcExtTyVarCount :: DataCon -> Int
dcExtTyVarCount dc = case dcSig dc of
  DataConSig _ ext _ -> Scoped.teleLength ext

-- | The number of fields of a data constructor
dcFieldCount :: DataCon -> Int
dcFieldCount dc = case dcSig dc of
  DataConSig _ _ fields -> length fields

instance Show DataCon where
  show = show . dcName

-- | Doesn't force the types, which are derived lazily (see 'DataCon')
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
