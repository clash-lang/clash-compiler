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
  , mkDataCon
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
  }

-- | A data constructor with the given name, tag, closed type, signature (see
-- "Clash.Core.Scoped.Sig"), strictness of its fields, and labels of its
-- fields
mkDataCon
  :: DcName -> ConTag -> Scoped.Type 'Scoped.VoidS -> DataConSig -> [DcStrictness]
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
