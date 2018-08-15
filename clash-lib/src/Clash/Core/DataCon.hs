{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                          2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Data Constructors in CoreHW
-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Clash.Core.DataCon
  ( DataCon (..)
  , DcName
  , ConTag
  , dataConInstArgTys
  )
where

#ifndef MIN_VERSION_unbound_generics
#define MIN_VERSION_unbound_generics(x,y,z)(1)
#endif

import Control.DeepSeq                        (NFData(..))
import Data.Binary                            (Binary)
import Data.Hashable                          (Hashable)
import GHC.Generics                           (Generic)

import Clash.Core.Name                        (Name (..))
import {-# SOURCE #-} Clash.Core.Subst        (substTyWith)
import {-# SOURCE #-} Clash.Core.Type         (Type)
import Clash.Core.Var                         (TyVar)
import Clash.Unique
import Clash.Util

-- | Data Constructor
data DataCon
  = MkData
  { dcName       :: !DcName  -- ^ Name of the DataCon
  , dcUniq       :: {-# UNPACK #-} !Unique
  , dcTag        :: !ConTag  -- ^ Syntactical position in the type definition
  , dcType       :: !Type    -- ^ Type of the 'DataCon
  , dcUnivTyVars :: [TyVar]  -- ^ Universally quantified type-variables,
                             -- these type variables are also part of the
                             -- result type of the DataCon
  , dcExtTyVars  :: [TyVar]  -- ^ Existentially quantified type-variables,
                             -- these type variables are not part of the result
                             -- of the DataCon, but only of the arguments.
  , dcArgTys     :: [Type]   -- ^ Argument types
  } deriving (Generic,NFData,Hashable,Binary)

instance Show DataCon where
  show = show . dcName

instance Eq DataCon where
  (==) = (==) `on` dcUniq
  (/=) = (/=) `on` dcUniq

instance Ord DataCon where
  compare = compare `on` dcUniq

instance Uniquable DataCon where
  getUnique = dcUniq

-- | Syntactical position of the DataCon in the type definition
type ConTag = Int
-- | DataCon reference
type DcName = Name DataCon

-- | Given a DataCon and a list of types, the type variables of the DataCon
-- type are substituted for the list of types. The argument types are returned.
--
-- The list of types should be equal to the number of type variables, otherwise
-- @Nothing@ is returned.
dataConInstArgTys :: DataCon -> [Type] -> Maybe [Type]
dataConInstArgTys (MkData { dcArgTys     = arg_tys
                          , dcUnivTyVars = univ_tvs
                          , dcExtTyVars  = ex_tvs
                          })
                  inst_tys
  | length tyvars == length inst_tys
  = Just (map (substTyWith tyvars inst_tys) arg_tys)

  | otherwise
  = Nothing

  where
    tyvars = univ_tvs ++ ex_tvs
