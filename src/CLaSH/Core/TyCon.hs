{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.TyCon where

-- External Import
import Unbound.LocallyNameless as Unbound

-- Internal Imports
import {-# SOURCE #-} CLaSH.Core.DataCon  (DataCon)
import {-# SOURCE #-} CLaSH.Core.Term     (Term)
import {-# SOURCE #-} CLaSH.Core.TypeRep  (Kind,Type,TyName)

data TyCon
  = AlgTyCon
  { tyConName   :: TyConName
  , tyConKind   :: Kind
  , tyConArity  :: Int
  , tyConTyVars :: [TyName]
  , algTcRhs    :: AlgTyConRhs
  , algTcParent :: TyConParent
  }

  | TupleTyCon
  { tyConName   :: TyConName
  , tyConKind   :: Kind
  , tyConArity  :: Int
  , tyConTyVars :: [TyName]
  , dataCon     :: DataCon
  }

  | SuperKindTyCon
  { tyConName   :: TyConName
  }

  | PrimTyCon
  { tyConName    :: TyConName
  , tyConKind    :: Kind
  , tyConArity   :: Int
  , primTyConRep :: PrimRep
  }
  deriving (Eq,Ord,Show)

type TyConName = Name TyCon

data AlgTyConRhs
  = DataTyCon
  { data_cons :: [DataCon]
  }
  | NewTyCon
  { data_con :: DataCon
  }
  deriving (Eq,Ord,Show)

data TyConParent
  = NoParentTyCon
  | ClassTyCon
  deriving (Eq,Ord,Show)

data PrimRep
  = AddrRep
  | IntRep
  | VoidRep
  deriving (Eq,Ord,Show)

Unbound.derive [''TyCon,''AlgTyConRhs,''PrimRep,''TyConParent]

instance Alpha PrimRep
instance Alpha TyCon where
  fv' _ _        = emptyC
  aeq' _ tc1 tc2 = aeq (tyConName tc1) (tyConName tc2)

instance Alpha AlgTyConRhs
instance Alpha TyConParent

instance Subst Type TyCon
instance Subst Type AlgTyConRhs
instance Subst Type PrimRep
instance Subst Type TyConParent

instance Subst Term TyCon
instance Subst Term AlgTyConRhs
instance Subst Term PrimRep
instance Subst Term TyConParent

mkKindTyCon ::
  TyConName
  -> Kind
  -> TyCon
mkKindTyCon name kind
  = mkPrimTyCon name kind 0 VoidRep

mkSuperKindTyCon ::
  TyConName
  -> TyCon
mkSuperKindTyCon name = SuperKindTyCon name

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

isTupleTyCon :: TyCon -> Bool
isTupleTyCon (TupleTyCon {}) = True
isTupleTyCon _               = False

isClassTyCon :: TyCon -> Bool
isClassTyCon (AlgTyCon {algTcParent = ClassTyCon}) = True
isClassTyCon _                                     = False

isSuperKindTyCon :: TyCon -> Bool
isSuperKindTyCon (SuperKindTyCon {}) = True
isSuperKindTyCon _                   = False

isTupleTyConLike :: TyCon -> Bool
isTupleTyConLike (TupleTyCon {}) = True
isTupleTyConLike (AlgTyCon {tyConName = nm}) = tupleName (name2String nm)
  where
    tupleName nm
      | '(' <- head nm
      , ')' <- last nm
      = all (== ',') (init $ tail nm)
    tupleName _ = False

isTupleTyConLike _ = False
