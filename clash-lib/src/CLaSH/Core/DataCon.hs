{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Data Constructors in CoreHW
module CLaSH.Core.DataCon
  ( DataCon (..)
  , DcName
  , ConTag
  , dataConInstArgTys
  )
where

import                Data.Monoid                           (mempty)
import                Data.Typeable
import                Control.DeepSeq
import                GHC.Generics
import                Unbound.Generics.LocallyNameless      as Unbound
import                Unbound.Generics.LocallyNameless.Name (Name(..))

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
  } deriving (Generic,Typeable)

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

instance Alpha DataCon where
  aeq' c dc1 dc2      = aeq' c (dcName dc1) (dcName dc2)

  fvAny' _ _ dc       = pure dc

  close _ _ dc        = dc
  open _ _ dc         = dc

  isPat _             = mempty
  isTerm _            = True

  nthPatFind _        = Left
  namePatFind _ _     = Left 0

  swaps' _ _ dc       = dc
  lfreshen' _ dc cont = cont dc mempty
  freshen' _ dc       = return (dc,mempty)

instance Subst Type DataCon
instance Subst Term DataCon

instance NFData DataCon where
  rnf dc = case dc of
    MkData nm tag ty uv ev args -> rnf nm `seq` rnf tag `seq` rnf ty `seq`
                                   rnf uv `seq` rnf ev `seq` rnf args

instance NFData (Name DataCon) where
  rnf nm = case nm of
    (Fn s i) -> rnf s `seq` rnf i
    (Bn l r) -> rnf l `seq` rnf r

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
