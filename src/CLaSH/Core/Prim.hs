{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.Prim where

import Unbound.LocallyNameless as Unbound

import CLaSH.Core.DataCon             (DataCon)
import {-# SOURCE #-} CLaSH.Core.Term (Term)
import CLaSH.Core.Type                (Type)
import CLaSH.Core.Var                 (Id)

data Prim
  = PrimFun  Id
  | PrimCon  DataCon
  | PrimDict Id
  | PrimDFun Id
  | PrimCo   Type
  deriving Show

Unbound.derive [''Prim]

instance Alpha Prim

instance Subst Type Prim
instance Subst Term Prim

primType ::
  Prim
  -> Type
primType _ = error "primType"

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
    numFuns = ["+","*","-","negate","abs","signum","fromInteger"]
