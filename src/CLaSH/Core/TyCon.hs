{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.TyCon
  ( TyCon (..)
  , TyConName
  , AlgTyConRhs (..)
  , PrimRep (..)
  , mkKindTyCon
  , isTupleTyConLike
  , tyConDataCons
  )
where

-- External Import
import                Unbound.LocallyNameless as Unbound

-- Internal Imports
import {-# SOURCE #-} CLaSH.Core.DataCon      (DataCon)
import {-# SOURCE #-} CLaSH.Core.Term         (Term)
import {-# SOURCE #-} CLaSH.Core.Type         (Kind, TyName, Type)
import                CLaSH.Util

data TyCon
  -- | Algorithmic DataCons
  = AlgTyCon
  { tyConName   :: TyConName   -- ^ Name of the TyCon
  , tyConKind   :: Kind        -- ^ Kind of the TyCon
  , tyConArity  :: Int         -- ^ Number of type arguments
  , algTcRhs    :: AlgTyConRhs -- ^ DataCon definitions
  }
  -- | Primitive TyCons
  | PrimTyCon
  { tyConName    :: TyConName  -- ^ Name of the TyCon
  , tyConKind    :: Kind       -- ^ Kind of the TyCon
  , tyConArity   :: Int        -- ^ Number of type arguments
  , primTyConRep :: PrimRep    -- ^ Representation
  }
  -- | To close the loop on the type hierarchy
  | SuperKindTyCon
  { tyConName :: TyConName     -- ^ Name of the TyCon
  }

instance Show TyCon where
  show (AlgTyCon       {tyConName = n}) = "AlgTyCon: " ++ show n
  show (PrimTyCon      {tyConName = n}) = "PrimTyCon: " ++ show n
  show (SuperKindTyCon {tyConName = n}) = "SuperKindTyCon: " ++ show n

instance Eq TyCon where
  (==) = (==) `on` tyConName

instance Ord TyCon where
  compare = compare `on` tyConName

type TyConName = Name TyCon

data AlgTyConRhs
  = DataTyCon
  { dataCons :: [DataCon]        -- ^ The DataCons of a TyCon
  }
  | NewTyCon
  { dataCon   :: DataCon         -- ^ The newtype DataCon
  , ntEtadRhs :: ([TyName],Type) -- ^ The argument type of the newtype
                                 -- DataCon in eta-reduced form, which is
                                 -- just the representation of the TyCon.
                                 -- The TyName's are the type-variables from
                                 -- the corresponding TyCon.
  }
  deriving Show

data PrimRep
  = IntRep
  | VoidRep
  deriving Show

Unbound.derive [''TyCon,''AlgTyConRhs,''PrimRep]

instance Alpha PrimRep
instance Alpha TyCon where
  swaps' _ _ d    = d
  fv' _ _         = emptyC
  lfreshen' _ a f = f a empty
  freshen' _ a    = return (a,empty)
  aeq' _ tc1 tc2  = aeq (tyConName tc1) (tyConName tc2)
  acompare' _ tc1 tc2 = acompare (tyConName tc1) (tyConName tc2)
  open _ _ d      = d
  close _ _ d     = d
  isPat _         = error "isPat TyCon"
  isTerm _        = error "isTerm TyCon"
  isEmbed _       = error "isEmbed TyCon"
  nthpatrec _     = error "nthpatrec TyCon"
  findpatrec _ _  = error "findpatrec TyCon"

instance Alpha AlgTyConRhs

instance Subst Type TyCon
instance Subst Type AlgTyConRhs
instance Subst Type PrimRep

instance Subst Term TyCon
instance Subst Term AlgTyConRhs
instance Subst Term PrimRep

-- | Create a Kind out of a TyConName
mkKindTyCon :: TyConName
            -> Kind
            -> TyCon
mkKindTyCon name kind
  = PrimTyCon name kind 0 VoidRep

-- | Does the TyCon look like a tuple TyCon
isTupleTyConLike :: TyCon -> Bool
isTupleTyConLike (AlgTyCon {tyConName = nm}) = tupleName (name2String nm)
  where
    tupleName nm
      | '(' <- head nm
      , ')' <- last nm
      = all (== ',') (init $ tail nm)
    tupleName _ = False

isTupleTyConLike _ = False

-- | Get the DataCons belonging to a TyCon
tyConDataCons :: TyCon -> [DataCon]
tyConDataCons (AlgTyCon {algTcRhs = DataTyCon { dataCons = cons}}) = cons
tyConDataCons (AlgTyCon {algTcRhs = NewTyCon  { dataCon  = con }}) = [con]
tyConDataCons _                                                    = []
