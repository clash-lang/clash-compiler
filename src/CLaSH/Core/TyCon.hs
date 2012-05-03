{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.TyCon where

-- External Import
import Unbound.LocallyNameless as Unbound

-- Internal Imports
import {-# SOURCE #-} CLaSH.Core.DataCon  (DataCon)
import {-# SOURCE #-} CLaSH.Core.Term     (Term)
import {-# SOURCE #-} CLaSH.Core.TypeRep  (Kind,Type)
import CLaSH.Core.Var                     (TyVar)

data TyCon
  = AlgTyCon
  { tyConName   :: TyConName
  , tyConKind   :: Kind
  , tyConTyVars :: [TyVar]
  , algTcRhs    :: AlgTyConRhs
  }

  | SynTyCon
  { tyConName   :: TyConName
  , tyConKind   :: Kind
  , tyConTyVars :: [TyVar]
  , synTcRhs    :: SynTyConRhs
  }

  | TupleTyCon
  { tyConName   :: TyConName
  , tyConKind   :: Kind
  , tyConTyVars :: [TyVar]
  , dataCon     :: DataCon
  }

  | SuperKindTyCon
  { tyConName   :: TyConName
  }
  | PrimTyCon
  { tyConName    :: TyConName
  , tyConKind    :: Kind
  , primTyConRep :: PrimRep
  }
  deriving (Eq,Show)

type TyConName = Name TyCon

data AlgTyConRhs
  = DataTyCon
  { data_cons :: [DataCon]
  }
  | NewTyCon
  { data_con :: DataCon
  }
  deriving (Eq,Show)

data SynTyConRhs
  = SynonymTyCon Type
  deriving (Eq,Show)

data PrimRep
  = AddrRep
  | IntRep
  | VoidRep
  deriving (Eq,Show)

Unbound.derive [''TyCon,''AlgTyConRhs,''PrimRep,''SynTyConRhs]

instance Alpha PrimRep
instance Alpha TyCon
instance Alpha AlgTyConRhs
instance Alpha SynTyConRhs

instance Subst Type TyCon
instance Subst Type AlgTyConRhs
instance Subst Type PrimRep
instance Subst Type SynTyConRhs

instance Subst Term TyCon
instance Subst Term AlgTyConRhs
instance Subst Term PrimRep
instance Subst Term SynTyConRhs

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
mkPrimTyCon name kind _ rep
  = PrimTyCon
  { tyConName = name
  , tyConKind = kind
  , primTyConRep = rep
  }

isSynTyCon :: TyCon -> Bool
isSynTyCon (SynTyCon {}) = True
isSynTyCon _             = False

coreExpandTyCon_maybe ::
  TyCon
  -> [Type]
  -> Maybe ([(TyVar,Type)],Type,[Type])
coreExpandTyCon_maybe (SynTyCon { tyConTyVars = tvs
                                , synTcRhs = SynonymTyCon rhs }) tys
    = expand tvs rhs tys

coreExpandTyCon_maybe _ _ = Nothing

expand ::
  [TyVar]
  -> Type
  -> [Type]
  -> Maybe ([(TyVar,Type)], Type, [Type])
expand tvs rhs tys =
    case n_tvs `compare` length tys of
      LT -> Just (tvs `zip` tys, rhs, drop n_tvs tys)
      EQ -> Just (tvs `zip` tys, rhs, [])
      Prelude.GT -> Nothing
  where
    n_tvs = length tvs
