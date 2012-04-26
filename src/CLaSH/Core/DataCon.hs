{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.DataCon where

import Unbound.LocallyNameless as Unbound

import {-# SOURCE #-} CLaSH.Core.Term (Term)
import CLaSH.Core.Type                (Type)
import CLaSH.Core.Var                 (Id,TyVar)

data DataCon
  = MkData
  { dcName       :: DcName
  , dcTag        :: ConTag
  , dcRepArgTys  :: [Type]
  , dcUnivTyVars :: [TyVar]
  , dcWorkId     :: Id
  }
  deriving (Eq,Show)

type ConTag = Int
type DcName = Name DataCon

Unbound.derive [''DataCon]

instance Alpha DataCon
instance Subst Type DataCon
instance Subst Term DataCon
