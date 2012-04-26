{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.TyCon where

-- External Import
import Unbound.LocallyNameless as Unbound

-- Internal Imports
import CLaSH.Core.DataCon             (DataCon)
import {-# SOURCE #-} CLaSH.Core.Term (Term)
import {-# SOURCE #-} CLaSH.Core.Type (Type,Kind)
import CLaSH.Core.Var                 (TyVar)

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

data PrimRep
  = AddrRep
  | IntRep
  deriving (Eq,Show)

Unbound.derive [''TyCon,''AlgTyConRhs,''PrimRep]

instance Alpha TyCon
instance Alpha AlgTyConRhs

instance Subst Type TyCon
instance Subst Type AlgTyConRhs

instance Subst Term TyCon
instance Subst Term AlgTyConRhs

pcPrimTyCon0 ::
  TyConName
  -> PrimRep
  -> TyCon
pcPrimTyCon0 name rep
  = mkPrimTyCon name result_kind 0 rep
  where
    result_kind = unliftedTypeKind

mkPrimTyCon :: TyConName  -> Kind -> Int -> PrimRep -> TyCon
mkPrimTyCon name _ _ rep
  = PrimTyCon
  { tyConName = name
  , primTyConRep = rep
  }
