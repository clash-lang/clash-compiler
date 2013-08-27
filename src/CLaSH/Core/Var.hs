{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
{-# OPTIONS_GHC -fno-warn-duplicate-constraints #-}
#endif

module CLaSH.Core.Var where

import                Unbound.LocallyNameless      as Unbound
import                Unbound.LocallyNameless.Name (isFree)

import {-# SOURCE #-} CLaSH.Core.Term              (Term)
import {-# SOURCE #-} CLaSH.Core.Type              (Kind, Type)
import                CLaSH.Util

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

modifyVarName ::
  (Name a -> Name a)
  -> Var a
  -> Var a
modifyVarName f (TyVar n k) = TyVar (f n) k
modifyVarName f (Id n t)    = Id (f n) t
