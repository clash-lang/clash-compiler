{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.DataCon where

import Unbound.LocallyNameless as Unbound

import {-# SOURCE #-} CLaSH.Core.Term (Term,TmName)
import {-# SOURCE #-} CLaSH.Core.Type (Type,TyName)
import CLaSH.Util

data DataCon
  = MkData
  { dcName       :: DcName
  , dcTag        :: ConTag
  , dcRepArgTys  :: [Type]
  , dcUnivTyVars :: [TyName]
  , dcWorkId     :: (TmName, Type)
  }

instance Show DataCon where
  show dc = show (dcName dc)

instance Eq DataCon where
  dc1 == dc2 = (dcName dc1) == (dcName dc2)

instance Ord DataCon where
  compare dc1 dc2 = compare (dcName dc1) (dcName dc2)

type ConTag = Int
type DcName = Name DataCon

Unbound.derive [''DataCon]

instance Alpha DataCon where
  swaps' _ _ d    = d
  fv' _ _         = emptyC
  lfreshen' _ a f = f a empty
  freshen' _ a    = return (a,empty)
  aeq' _ dc1 dc2  = aeq (dcName dc1) (dcName dc2)
  acompare' _ _ _ = error "acompare' DataCon"
  open _ _ d      = d
  close _ _ d     = d
  isPat _         = error "isPat DataCon"
  isTerm _        = error "isTerm DataCon"
  isEmbed _       = error "isEmbed DataCon"
  nthpatrec _     = error "nthpatrec DataCon"
  findpatrec _ _  = error "findpatrec DataCon"

instance Subst Type DataCon
instance Subst Term DataCon

dataConInstArgTys :: DataCon -> [Type] -> [Type]
dataConInstArgTys (MkData { dcRepArgTys  = rep_arg_tys
                          , dcUnivTyVars = univ_tvs
                          })
                  inst_tys
  | length univ_tvs == length inst_tys
  = map (substs (zip univ_tvs inst_tys)) rep_arg_tys

  | otherwise
  = error $ $(curLoc) ++ "dataConInstArgTys: number of tyVars and Types differ"
