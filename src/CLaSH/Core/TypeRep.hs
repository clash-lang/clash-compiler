{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.TypeRep
  ( Type (..)
  , Kind
  , SuperKind
  , TyName
  , KiName
  , mkTyConTy
  )
where

-- External imports
import Data.Hashable (Hashable(..))
import Unbound.LocallyNameless as Unbound
import Unbound.LocallyNameless.Name

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

type Kind = Type
type SuperKind = Type

type TyName = Name Type
type KiName = Name Type

Unbound.derive [''Type]

instance Hashable (Name Type) where
  hash (Nm _ (str, int)) = hashWithSalt (hash int) str
  hash (Bn _ i0 i1)      = hash i0 `hashWithSalt` i1

instance Alpha Type

instance Subst Term Type
instance Subst Type Type where
  isvar (TyVarTy v) = Just (SubstName v)
  isvar _           = Nothing

instance Eq Type where
  (==) = aeq

mkTyConTy :: TyCon -> Type
mkTyConTy tycon = TyConApp tycon []
