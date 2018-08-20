{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Type Constructors in CoreHW
-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Clash.Core.TyCon
  ( TyCon (..)
  , TyConName
  , TyConOccName
  , TyConMap
  , AlgTyConRhs (..)
  , mkKindTyCon
  , isTupleTyConLike
  , tyConDataCons
  )
where

#ifndef MIN_VERSION_unbound_generics
#define MIN_VERSION_unbound_generics(x,y,z)(1)
#endif

-- External Import
import Control.DeepSeq
import Data.Binary                            (Binary)
import Data.HashMap.Lazy                      (HashMap)
import GHC.Generics
import Unbound.Generics.LocallyNameless       (Alpha(..))
import Unbound.Generics.LocallyNameless.Extra ()
#if MIN_VERSION_unbound_generics(0,3,0)
import Data.Monoid                            (All (..))
import Unbound.Generics.LocallyNameless       (NthPatFind (..),
                                               NamePatFind (..))
#endif

-- Internal Imports
import Clash.Core.DataCon                     (DataCon)
import Clash.Core.Name
import {-# SOURCE #-} Clash.Core.Type         (Kind, TyName, Type)
import Clash.Util

-- | Type Constructor
data TyCon
  -- | Algorithmic DataCons
  = AlgTyCon
  { tyConName   :: !TyConName   -- ^ Name of the TyCon
  , tyConKind   :: !Kind        -- ^ Kind of the TyCon
  , tyConArity  :: !Int         -- ^ Number of type arguments
  , algTcRhs    :: !AlgTyConRhs -- ^ DataCon definitions
  }
  -- | Function TyCons (e.g. type families)
  | FunTyCon
  { tyConName   :: !TyConName      -- ^ Name of the TyCon
  , tyConKind   :: !Kind           -- ^ Kind of the TyCon
  , tyConArity  :: !Int            -- ^ Number of type arguments
  , tyConSubst  :: [([Type],Type)] -- ^ List of: ([LHS match types], RHS type)
  }
  -- | Primitive TyCons
  | PrimTyCon
  { tyConName    :: !TyConName  -- ^ Name of the TyCon
  , tyConKind    :: !Kind       -- ^ Kind of the TyCon
  , tyConArity   :: !Int        -- ^ Number of type arguments
  }
  -- | To close the loop on the type hierarchy
  | SuperKindTyCon
  { tyConName :: !TyConName     -- ^ Name of the TyCon
  }
  deriving (Generic,NFData,Binary)

instance Show TyCon where
  show (AlgTyCon       {tyConName = n}) = "AlgTyCon: " ++ show n
  show (FunTyCon       {tyConName = n}) = "FunTyCon: " ++ show n
  show (PrimTyCon      {tyConName = n}) = "PrimTyCon: " ++ show n
  show (SuperKindTyCon {tyConName = n}) = "SuperKindTyCon: " ++ show n

instance Eq TyCon where
  (==) = (==) `on` tyConName

instance Ord TyCon where
  compare = compare `on` tyConName

-- | TyCon reference
type TyConName = Name TyCon
type TyConOccName = OccName TyCon
type TyConMap = HashMap TyConOccName TyCon

-- | The RHS of an Algebraic Datatype
data AlgTyConRhs
  = DataTyCon
  { dataCons :: [DataCon]        -- ^ The DataCons of a TyCon
  }
  | NewTyCon
  { dataCon   :: !DataCon        -- ^ The newtype DataCon
  , ntEtadRhs :: ([TyName],Type) -- ^ The argument type of the newtype
                                 -- DataCon in eta-reduced form, which is
                                 -- just the representation of the TyCon.
                                 -- The TyName's are the type-variables from
                                 -- the corresponding TyCon.
  }
  deriving (Show,Generic,NFData,Alpha,Binary)

instance Alpha TyCon where
  aeq' c tc1 tc2      = aeq' c (tyConName tc1) (tyConName tc2)

  fvAny' _ _ tc       = pure tc

  close _ _ tc        = tc
  open _ _ tc         = tc

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

  swaps' _ _ tc       = tc
  lfreshen' _ tc cont = cont tc mempty
  freshen' _ tc       = return (tc,mempty)

  acompare' c tc1 tc2 = acompare' c (tyConName tc1) (tyConName tc2)

-- | Create a Kind out of a TyConName
mkKindTyCon :: TyConName
            -> Kind
            -> TyCon
mkKindTyCon name kind
  = PrimTyCon name kind 0

-- | Does the TyCon look like a tuple TyCon
isTupleTyConLike :: TyConName -> Bool
isTupleTyConLike nm = tupleName (name2String nm)
  where
    tupleName nm'
      | '(' <- head nm'
      , ')' <- last nm'
      = all (== ',') (init $ tail nm')
    tupleName _ = False

-- | Get the DataCons belonging to a TyCon
tyConDataCons :: TyCon -> [DataCon]
tyConDataCons (AlgTyCon {algTcRhs = DataTyCon { dataCons = cons}}) = cons
tyConDataCons (AlgTyCon {algTcRhs = NewTyCon  { dataCon  = con }}) = [con]
tyConDataCons _                                                    = []
