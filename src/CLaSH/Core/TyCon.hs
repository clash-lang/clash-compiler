{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.TyCon where

-- External Import
import                Unbound.LocallyNameless as Unbound

-- Internal Imports
import {-# SOURCE #-} CLaSH.Core.DataCon      (DataCon)
import {-# SOURCE #-} CLaSH.Core.Term         (Term)
import {-# SOURCE #-} CLaSH.Core.Type         (Kind, TyName, Type)
import                CLaSH.Util

data TyCon
  = AlgTyCon
  { tyConName   :: TyConName
  , tyConKind   :: Kind
  , tyConArity  :: Int
  , algTcRhs    :: AlgTyConRhs
  , isDictTyCon :: Bool
  }

  | PrimTyCon
  { tyConName    :: TyConName
  , tyConKind    :: Kind
  , tyConArity   :: Int
  , primTyConRep :: PrimRep
  }

  | SuperKindTyCon
  { tyConName :: TyConName
  }

instance Show TyCon where
  show = show . tyConName

instance Eq TyCon where
  (==) = (==) `on` tyConName

instance Ord TyCon where
  compare = compare `on` tyConName

type TyConName = Name TyCon

data AlgTyConRhs
  = DataTyCon
  { data_cons :: [DataCon]
  }
  | NewTyCon
  { data_con    :: DataCon
  , nt_etad_rhs :: ([TyName],Type)
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

mkKindTyCon ::
  TyConName
  -> Kind
  -> TyCon
mkKindTyCon name kind
  = mkPrimTyCon name kind 0 VoidRep

mkSuperKindTyCon ::
  TyConName
  -> TyCon
mkSuperKindTyCon = SuperKindTyCon

mkPrimTyCon ::
  TyConName
  -> Kind
  -> Int
  -> PrimRep
  -> TyCon
mkPrimTyCon name kind arity rep
  = PrimTyCon
  { tyConName = name
  , tyConKind = kind
  , tyConArity = arity
  , primTyConRep = rep
  }

isSuperKindTyCon :: TyCon -> Bool
isSuperKindTyCon (SuperKindTyCon {}) = True
isSuperKindTyCon _                   = False

isTupleTyConLike :: TyCon -> Bool
isTupleTyConLike (AlgTyCon {tyConName = nm}) = tupleName (name2String nm)
  where
    tupleName nm
      | '(' <- head nm
      , ')' <- last nm
      = all (== ',') (init $ tail nm)
    tupleName _ = False

isTupleTyConLike _ = False

tyConDataCons :: TyCon -> [DataCon]
tyConDataCons (AlgTyCon {algTcRhs = DataTyCon { data_cons = cons}}) = cons
tyConDataCons (AlgTyCon {algTcRhs = NewTyCon  { data_con  = con }}) = [con]
tyConDataCons _                                                     = []
