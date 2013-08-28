{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.Prim
  ( Prim (..)
  , primType
  )
where

import                Unbound.LocallyNameless as Unbound

import                CLaSH.Core.DataCon      (DataCon, dcType)
import {-# SOURCE #-} CLaSH.Core.Term         (Term, TmName)
import {-# SOURCE #-} CLaSH.Core.Type         (Type)

data Prim
  = PrimFun  TmName Type -- ^ Primitive Function
  | PrimCon  DataCon     -- ^ Primitive DataConstructor
  | PrimDict TmName Type -- ^ Primitive Dictionary
  | PrimDFun TmName Type -- ^ Primitive Dictionary Function
  | PrimCo   Type        -- ^ Mainly there to deal with GHC/SystemFC coercions
  deriving (Eq,Ord,Show)

Unbound.derive [''Prim]

instance Alpha Prim where
  fv' _ _ = emptyC

instance Subst Type Prim
instance Subst Term Prim

-- | Determines the Type of a Literal
primType ::
  Prim
  -> Type
primType p = case p of
  PrimFun _ t  -> t
  PrimCon dc   -> dcType dc
  PrimDict _ t -> t
  PrimDFun _ t -> t
  PrimCo t     -> t
