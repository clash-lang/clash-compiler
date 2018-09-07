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
import Unbound.Generics.LocallyNameless       (Alpha(..),Subst(..))
import Unbound.Generics.LocallyNameless.Extra ()
#if MIN_VERSION_unbound_generics(0,3,0)
import Data.Monoid                            (All (..))
import Unbound.Generics.LocallyNameless       (NthPatFind (..),
                                               NamePatFind (..))
#endif

import Clash.Core.Name                        (Name (..))
import {-# SOURCE #-} Clash.Core.Type         (TyName, Type)
import Clash.Util

-- | Data Constructor
data DataCon
  = MkData
  { dcName       :: !DcName  -- ^ Name of the DataCon
  , dcTag        :: !ConTag  -- ^ Syntactical position in the type definition
  , dcType       :: !Type    -- ^ Type of the 'DataCon
  , dcUnivTyVars :: [TyName] -- ^ Universally quantified type-variables,
                             -- these type variables are also part of the
                             -- result type of the DataCon
  , dcExtTyVars  :: [TyName] -- ^ Existentially quantified type-variables,
                             -- these type variables are not part of the result
                             -- of the DataCon, but only of the arguments.
  , dcArgTys     :: [Type]   -- ^ Argument types
  } deriving (Generic,NFData,Hashable,Binary)

instance Show DataCon where
  show = show . dcName

instance Eq DataCon where
  (==) = (==) `on` dcName

instance Ord DataCon where
  compare = compare `on` dcName

-- | Syntactical position of the DataCon in the type definition
type ConTag = Int
-- | DataCon reference
type DcName = Name DataCon

instance Alpha DataCon where
  aeq' c dc1 dc2      = aeq' c (dcName dc1) (dcName dc2)

  fvAny' _ _ dc       = pure dc

  close _ _ dc        = dc
  open _ _ dc         = dc

  isPat _             = mempty

#if MIN_VERSION_unbound_generics(0,3,0)
  isTerm _            = All True
  nthPatFind _        = NthPatFind Left
  namePatFind _       = NamePatFind (const (Left 0))
#else
  isTerm _            = True
  nthPatFind _        = Left
  namePatFind _ _     = Left 0
#endif

  swaps' _ _ dc       = dc
  lfreshen' _ dc cont = cont dc mempty
  freshen' _ dc       = return (dc,mempty)

  acompare' c dc1 dc2 = acompare' c (dcName dc1) (dcName dc2)

instance Subst a DataCon where
  subst _ _ dc = dc
  substs _ dc  = dc

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
  = Just (map (substs (zip tyvars inst_tys)) arg_tys)

  | otherwise
  = Nothing

  where
    tyvars = map nameOcc (univ_tvs ++ ex_tvs)
