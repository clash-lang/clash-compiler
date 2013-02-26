{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CLaSH.Core.Literal where

import Unbound.LocallyNameless as Unbound
import Unbound.LocallyNameless.Alpha

import {-# SOURCE #-} CLaSH.Core.Term (Term)
import {-# SOURCE #-} CLaSH.Core.Type (Type)
import CLaSH.Core.TysPrim             (intPrimTy,addrPrimTy)

data Literal
  = IntegerLiteral Integer
  | StringLiteral  String
  deriving (Eq,Ord,Show)

Unbound.derive [''Literal]

instance Alpha Literal where
  acompare' _ (IntegerLiteral i) (IntegerLiteral j) = compare i j
  acompare' c l1 l2 = acompareR1 rep1 c l1 l2

instance Subst Type Literal
instance Subst Term Literal

literalType ::
  Literal
  -> Type
literalType (IntegerLiteral _) = intPrimTy
literalType (StringLiteral  _) = addrPrimTy
