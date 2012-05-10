{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.DataCon where

import Unbound.LocallyNameless as Unbound

import {-# SOURCE #-} CLaSH.Core.Term (Term,TmName)
import CLaSH.Core.TypeRep                (Type,TyName)
import CLaSH.Util

data DataCon
  = MkData
  { dcName       :: DcName
  , dcTag        :: ConTag
  , dcRepArgTys  :: [Type]
  , dcUnivTyVars :: [TyName]
  , dcWorkId     :: Maybe (TmName, Type)
  }
  deriving (Eq,Ord,Show)

type ConTag = Int
type DcName = Name DataCon

Unbound.derive [''DataCon]

instance Alpha DataCon where
  fv' _ _        = emptyC
  aeq' _ dc1 dc2 = aeq (dcName dc1) (dcName dc2)

instance Subst Type DataCon
instance Subst Term DataCon

dataConWorkId ::
  DataCon
  -> (TmName, Type)
dataConWorkId (MkData { dcWorkId = dcIdM }) =
  case dcIdM of
    Nothing   -> error $ $(curLoc) ++ "No WorkerID for DataCon"
    Just dcId -> dcId
