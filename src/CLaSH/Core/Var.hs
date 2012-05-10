{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.Var where

import Unbound.LocallyNameless as Unbound
import Unbound.LocallyNameless.Name (isFree)

import {-# SOURCE #-} CLaSH.Core.TypeRep (Type,Kind)
import {-# SOURCE #-} CLaSH.Core.Term    (Term)
import CLaSH.Util

data Var a
  = TyVar
  { varName :: Name a
  , varKind :: Embed Kind
  }
  | Id
  { varName :: Name a
  , varType :: Embed Type
  }
  deriving (Eq,Ord,Show)

type Id    = Var Term
type TyVar = Var Type

Unbound.derive [''Var]

instance Alpha a => Alpha (Var a)

instance Subst Term Id
instance Subst Term TyVar

instance Subst Type TyVar
instance Subst Type Id where
  subst tvN u (Id idN ty) | isFree tvN = Id idN (subst tvN u ty)
  subst m _ _ = error $ $(curLoc) ++ "Cannot substitute for bound variable: " ++ show m
