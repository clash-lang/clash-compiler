{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.Type where

-- External imports
import Unbound.LocallyNameless as Unbound

-- Local imports
import {-# SOURCE #-} CLaSH.Core.Term  (Term)
import {-# SOURCE #-} CLaSH.Core.TyCon (TyCon)
import CLaSH.Core.TysPrim
import CLaSH.Core.Var                  (TyVar)

data Type
  = TyVarTy  TyName
  | FunTy    Type  Type
  | ForAllTy (Bind TyVar Type)
  | TyConApp TyCon [Type]
  deriving Show

type TyName = Name Type

type Kind = Type

Unbound.derive [''Type]

instance Alpha Type

instance Subst Term Type
instance Subst Type Type where
  isvar (TyVarTy v) = Just (SubstName v)
  isvar _           = Nothing

instance Eq Type where
  (==) = aeq

intPrimTy :: Type
intPrimTy = mkTyConTy intPrimTyCon

addrPrimTy :: Type
addrPrimTy = mkTyConTy addrPrimTyCon

mkTyConTy :: TyCon -> Type
mkTyConTy tycon = TyConApp tycon []


