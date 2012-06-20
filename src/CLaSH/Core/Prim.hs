{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.Prim where

import Unbound.LocallyNameless as Unbound

import CLaSH.Core.DataCon             (DataCon,dcWorkId)
import {-# SOURCE #-} CLaSH.Core.Term (Term,TmName)
import CLaSH.Core.TypeRep             (Type)

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

primDicts :: [String]
primDicts = ["$dPositiveT","$dNaturalT","$dIntegerT"]

primDFuns :: [String]
primDFuns = ["$fShowUnsigned","$fEqInteger","$fPositiveTx"
            ,"$fNaturalTx","$fArrowComponent","$fArrowLoopComponent"
            ,"$fShowSigned","$fNumInt"
            ]

primDataCons :: [String]
primDataCons = ["I#","Int#","Signed","Unsigned"]

primFuns :: [String]
primFuns = concat
  [ numFuns
  ]
  where
    numFuns = []
