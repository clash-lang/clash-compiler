{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module CLaSH.Core.TypeRep
  ( Type (..)
  , Kind
  , KindOrType
  , SuperKind
  , TyName
  , mkTyConTy
  )
where

-- External imports
import Unbound.LocallyNameless as Unbound

-- Local imports
import {-# SOURCE #-} CLaSH.Core.Term
import CLaSH.Core.TyCon
import CLaSH.Core.Var

data Type
  = TyVarTy  TyName
  | FunTy    Type  Type
  | ForAllTy (Bind TyVar Type)
  | TyConApp TyCon [Type]
  deriving Show

type TyName = Name Type

type Kind = Type
type KindOrType = Type
type SuperKind = Type

Unbound.derive [''Type]

instance Alpha Type

instance Subst Term Type
instance Subst Type Type where
  isvar (TyVarTy v) = Just (SubstName v)
  isvar _           = Nothing

instance Eq Type where
  (==) = aeq

mkTyConTy :: TyCon -> Type
mkTyConTy tycon = TyConApp tycon []
