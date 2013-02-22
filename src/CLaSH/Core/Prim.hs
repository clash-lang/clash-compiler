{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.Prim where

import Unbound.LocallyNameless as Unbound

import CLaSH.Core.DataCon             (DataCon,dcWorkId)
import {-# SOURCE #-} CLaSH.Core.Term (Term,TmName)
import {-# SOURCE #-} CLaSH.Core.Type (Type)

data Prim
  = PrimFun  TmName Type
  | PrimCon  DataCon
  | PrimDict TmName Type
  | PrimDFun TmName Type
  | PrimCo   Type
  deriving (Eq,Ord,Show)

Unbound.derive [''Prim]

instance Alpha Prim where
  fv' _ _ = emptyC

instance Subst Type Prim
instance Subst Term Prim

primType ::
  Prim
  -> Type
primType p = case p of
  PrimFun _ t  -> t
  PrimCon dc   -> snd $ dcWorkId dc
  PrimDict _ t -> t
  PrimDFun _ t -> t
  PrimCo t     -> t
