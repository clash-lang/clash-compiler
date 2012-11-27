{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.TypeRep
  ( Type (..)
  , TyLit (..)
  , Kind
  , SuperKind
  , TyName
  , KiName
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
  | LitTy    TyLit
  deriving Show

data TyLit
  = NumTyLit Int
  | StrTyLit String
  deriving Show

type Kind = Type
type SuperKind = Type

type TyName = Name Type
type KiName = Name Type

Unbound.derive [''Type,''TyLit]

instance Alpha Type
instance Alpha TyLit

instance Subst Type TyLit
instance Subst Term TyLit
instance Subst Term Type
instance Subst Type Type where
  isvar (TyVarTy v) = Just (SubstName v)
  isvar _           = Nothing

instance Eq Type where
  (==) = aeq

instance Ord Type where
  compare = acompare

mkTyConTy :: TyCon -> Type
mkTyConTy tycon = TyConApp tycon []
