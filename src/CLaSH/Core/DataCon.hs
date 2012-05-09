{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.DataCon where

import Unbound.LocallyNameless as Unbound

import {-# SOURCE #-} CLaSH.Core.Term (Term,TmName)
import CLaSH.Core.Type                (Type,TyName)

data DataCon
  = MkData
  { dcName       :: DcName
  , dcTag        :: ConTag
  , dcRepArgTys  :: [Type]
  , dcUnivTyVars :: [TyName]
  , dcWorkId     :: (TmName, Type)
  }
  deriving (Eq,Show)

type ConTag = Int
type DcName = Name DataCon

Unbound.derive [''DataCon]

instance Alpha DataCon
instance Subst Type DataCon
instance Subst Term DataCon
