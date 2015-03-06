{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
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

-- | Variables in CoreHW
module CLaSH.Core.Var
  ( Var (..)
  , Id
  , TyVar
  , modifyVarName
  )
where

import                Control.DeepSeq
import                Data.Typeable
import                GHC.Generics
import                Unbound.Generics.LocallyNameless

import {-# SOURCE #-} CLaSH.Core.Term                  (Term)
import {-# SOURCE #-} CLaSH.Core.Type                  (Kind, Type)
import                CLaSH.Util

-- | Variables in CoreHW
data Var a
  -- | Constructor for type variables
  = TyVar
  { varName :: Name a
  , varKind :: Embed Kind
  }
  -- | Constructor for term variables
  | Id
  { varName :: Name a
  , varType :: Embed Type
  }
  deriving (Eq,Ord,Show,Generic,Typeable)

-- | Term variable
type Id    = Var Term
-- | Type variable
type TyVar = Var Type

instance (Typeable a, Alpha a) => Alpha (Var a)

instance Subst Term Id
instance Subst Term TyVar

instance Subst Type TyVar
instance Subst Type Id where
  subst tvN u (Id idN ty) | isFreeName tvN = Id idN (subst tvN u ty)
  subst m _ _ = error $ $(curLoc) ++ "Cannot substitute for bound variable: " ++ show m

instance NFData (Name a) => NFData (Var a) where
  rnf v = case v of
    TyVar nm ki -> rnf nm `seq` rnf (unembed ki)
    Id    nm ty -> rnf nm `seq` rnf (unembed ty)

-- | Change the name of a variable
modifyVarName ::
  (Name a -> Name a)
  -> Var a
  -> Var a
modifyVarName f (TyVar n k) = TyVar (f n) k
modifyVarName f (Id n t)    = Id (f n) t
