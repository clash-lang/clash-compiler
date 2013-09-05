{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Data Constructors in CoreHW
module CLaSH.Core.DataCon
  ( DataCon (..)
  , DcName
  , ConTag
  , dataConInstArgTys
  )
where

import                Unbound.LocallyNameless as Unbound

import {-# SOURCE #-} CLaSH.Core.Term         (Term)
import {-# SOURCE #-} CLaSH.Core.Type         (TyName, Type)
import                CLaSH.Util

-- | Data Constructor
data DataCon
  = MkData
  { dcName       :: DcName   -- ^ Name of the DataCon
  , dcTag        :: ConTag   -- ^ Syntactical position in the type definition
  , dcType       :: Type     -- ^ Type of the 'DataCon
  , dcUnivTyVars :: [TyName] -- ^ Universally quantified type-variables,
                             -- these type variables are also part of the
                             -- result type of the DataCon
  , dcExtTyVars  :: [TyName] -- ^ Existentially quantified type-variables,
                             -- these type variables are not part of the result
                             -- of the DataCon, but only of the arguments.
  , dcArgTys     :: [Type]   -- ^ Argument types
  }

instance Show DataCon where
  show = show . dcName

instance Eq DataCon where
  (==) = (==) `on` dcName

instance Ord DataCon where
  compare = compare `on` dcName

-- | Syntactical position of the DataCon in the type definition
type ConTag = Int
-- | DataCon reference
type DcName = Name DataCon

Unbound.derive [''DataCon]

instance Alpha DataCon where
  swaps' _ _ d    = d
  fv' _ _         = emptyC
  lfreshen' _ a f = f a empty
  freshen' _ a    = return (a,empty)
  aeq' c dc1 dc2  = aeq' c (dcName dc1) (dcName dc2)
  acompare' c dc1 dc2 = acompare' c (dcName dc1) (dcName dc2)
  open _ _ d      = d
  close _ _ d     = d
  isPat _         = error "isPat DataCon"
  isTerm _        = error "isTerm DataCon"
  isEmbed _       = error "isEmbed DataCon"
  nthpatrec _     = error "nthpatrec DataCon"
  findpatrec _ _  = error "findpatrec DataCon"

instance Subst Type DataCon
instance Subst Term DataCon

-- | Given a DataCon and a list of types, the type variables of the DataCon
-- type are substituted for the list of types. The argument types are returned.
--
-- The list of types should be equal to the number of type variables, otherwise
-- an error is reported.
dataConInstArgTys :: DataCon -> [Type] -> [Type]
dataConInstArgTys (MkData { dcArgTys     = arg_tys
                          , dcUnivTyVars = univ_tvs
                          , dcExtTyVars  = ex_tvs
                          })
                  inst_tys
  | length tyvars == length inst_tys
  = map (substs (zip tyvars inst_tys)) arg_tys

  | otherwise
  = error $ $(curLoc) ++ "dataConInstArgTys: number of tyVars and Types differ"

  where
    tyvars = univ_tvs ++ ex_tvs
