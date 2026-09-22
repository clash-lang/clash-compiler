{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                          2017, Google Inc.,
                          2021, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Data Constructors in CoreHW
-}
module Clash.Core.DataCon
  ( DataCon (..),
    DcName,
    ConTag,
    DcStrictness (..),
  )
where

import Clash.Core.Name (Name (..))
import {-# SOURCE #-} Clash.Core.Type (Type)
import Clash.Core.Var (TyVar)
import Clash.Unique
import Control.DeepSeq (NFData (..))
import Data.Binary (Binary)
import Data.Function (on)
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Text as Text
import GHC.Generics (Generic)

-- | Data Constructor
data DataCon
  = MkData
  { -- | Name of the DataCon
    dcName :: !DcName,
    -- | Invariant: forall x . dcUniq x ~ nameUniq (dcName x)
    dcUniq :: {-# UNPACK #-} !Unique,
    -- | Syntactical position in the type definition
    dcTag :: !ConTag,
    -- | Type of the 'DataCon
    dcType :: !Type,
    -- | Universally quantified type-variables, these type variables are also part
    -- of the result type of the DataCon
    dcUnivTyVars :: [TyVar],
    -- | Existentially quantified type-variables, these type variables are not
    -- part of the result of the DataCon, but only of the arguments.
    dcExtTyVars :: [TyVar],
    -- | Argument types
    dcArgTys :: [Type],
    -- | Argument strictness
    dcArgStrict :: [DcStrictness],
    -- | Names of fields. Used when data constructor is referring to a record type.
    dcFieldLabels :: [Text.Text]
  }
  deriving (Generic, NFData, Binary)

instance Show DataCon where
  show = show . dcName

instance Eq DataCon where
  (==) = (==) `on` dcUniq
  (/=) = (/=) `on` dcUniq

instance Ord DataCon where
  compare = compare `on` dcUniq

instance Hashable DataCon where
  hashWithSalt salt = hashWithSalt salt . dcUniq

instance Uniquable DataCon where
  getUnique = dcUniq
  setUnique dc u = dc {dcUniq = u, dcName = (dcName dc) {nameUniq = u}}

-- | Syntactical position of the DataCon in the type definition
type ConTag = Int

-- | DataCon reference
type DcName = Name DataCon

data DcStrictness
  = Strict
  | Lazy
  deriving (Generic, NFData, Eq, Hashable, Binary)
