{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Type Constructors in CoreHW
-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Clash.Core.TyCon
  ( TyCon (..)
  , TyConName
  , TyConMap
  , AlgTyConRhs (..)
  , mkKindTyCon
  , isTupleTyConLike
  , isNewTypeTc
  , tyConDataCons
  )
where

-- External Import
import Control.DeepSeq
import Data.Binary                            (Binary)
import qualified Data.Text as T
import GHC.Generics

-- Internal Imports
import Clash.Core.DataCon                     (DataCon)
import Clash.Core.Name
import {-# SOURCE #-} Clash.Core.Type         (Kind, Type)
import Clash.Core.Var                         (TyVar)
import Clash.Unique
import Clash.Util

-- | Type Constructor
data TyCon
  -- | Algorithmic DataCons
  = AlgTyCon
  { tyConUniq   :: {-# UNPACK #-} !Unique
  , tyConName   :: !TyConName   -- ^ Name of the TyCon
  , tyConKind   :: !Kind        -- ^ Kind of the TyCon
  , tyConArity  :: !Int         -- ^ Number of type arguments
  , algTcRhs    :: !AlgTyConRhs -- ^ DataCon definitions
  , isClassTc   :: !Bool        -- ^ Is this a class dictionary?
  }
  -- | Function TyCons (e.g. type families)
  | FunTyCon
  { tyConUniq   :: {-# UNPACK #-} !Unique
  , tyConName   :: !TyConName      -- ^ Name of the TyCon
  , tyConKind   :: !Kind           -- ^ Kind of the TyCon
  , tyConArity  :: !Int            -- ^ Number of type arguments
  , tyConSubst  :: [([Type],Type)] -- ^ List of: ([LHS match types], RHS type)
  }
  -- | Primitive TyCons
  | PrimTyCon
  { tyConUniq    :: {-# UNPACK #-} !Unique
  , tyConName    :: !TyConName  -- ^ Name of the TyCon
  , tyConKind    :: !Kind       -- ^ Kind of the TyCon
  , tyConArity   :: !Int        -- ^ Number of type arguments
  }
  -- | To close the loop on the type hierarchy
  | SuperKindTyCon
  { tyConUniq    :: {-# UNPACK #-} !Unique
  , tyConName    :: !TyConName  -- ^ Name of the TyCon
  }
  deriving (Generic,NFData,Binary)

instance Show TyCon where
  show (AlgTyCon       {tyConName = n}) = "AlgTyCon: " ++ show n
  show (FunTyCon       {tyConName = n}) = "FunTyCon: " ++ show n
  show (PrimTyCon      {tyConName = n}) = "PrimTyCon: " ++ show n
  show (SuperKindTyCon {tyConName = n}) = "SuperKindTyCon: " ++ show n

instance Eq TyCon where
  (==) = (==) `on` tyConUniq
  (/=) = (/=) `on` tyConUniq

instance Uniquable TyCon where
  getUnique = tyConUniq
  setUnique tyCon u = tyCon {tyConUniq=u}

-- | TyCon reference
type TyConName = Name TyCon
type TyConMap  = UniqMap TyCon

-- | The RHS of an Algebraic Datatype
data AlgTyConRhs
  = DataTyCon
  { dataCons :: [DataCon]        -- ^ The DataCons of a TyCon
  }
  | NewTyCon
  { dataCon   :: !DataCon        -- ^ The newtype DataCon
  , ntEtadRhs :: ([TyVar],Type)  -- ^ The argument type of the newtype
                                 -- DataCon in eta-reduced form, which is
                                 -- just the representation of the TyCon.
                                 -- The TyName's are the type-variables from
                                 -- the corresponding TyCon.
  }
  deriving (Show,Generic,NFData,Binary)

-- | Create a Kind out of a TyConName
mkKindTyCon :: TyConName
            -> Kind
            -> TyCon
mkKindTyCon name kind
  = PrimTyCon (nameUniq name) name kind 0

-- | Does the TyCon look like a tuple TyCon
isTupleTyConLike :: TyConName -> Bool
isTupleTyConLike nm = tupleName (nameOcc nm)
  where
    tupleName nm'
      | '(' <- T.head nm'
      , ')' <- T.last nm'
      = T.all (== ',') (T.init $ T.tail nm')
    tupleName _ = False

-- | Get the DataCons belonging to a TyCon
tyConDataCons :: TyCon -> [DataCon]
tyConDataCons (AlgTyCon {algTcRhs = DataTyCon { dataCons = cons}}) = cons
tyConDataCons (AlgTyCon {algTcRhs = NewTyCon  { dataCon  = con }}) = [con]
tyConDataCons _                                                    = []

isNewTypeTc
  :: TyCon
  -> Bool
isNewTypeTc (AlgTyCon {algTcRhs = NewTyCon {}}) = True
isNewTypeTc _ = False
