{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Term Literal
module CLaSH.Core.Literal
  ( Literal (..)
  , literalType
  )
where

import                Control.DeepSeq
import                GHC.Generics
import                Unbound.Generics.LocallyNameless

import {-# SOURCE #-} CLaSH.Core.Term               (Term)
import {-# SOURCE #-} CLaSH.Core.Type               (Type)
import                CLaSH.Core.TysPrim            (intPrimTy, voidPrimTy)
import                CLaSH.Util

-- | Term Literal
data Literal
  = IntegerLiteral  Integer
  | StringLiteral   String
  | RationalLiteral Rational
  deriving (Eq,Ord,Show,Generic)

instance Subst b Rational where
  subst  _ _ = id
  substs _   = id


instance Alpha Literal where
  fvAny' _ _ l = pure l

instance Subst Type Literal
instance Subst Term Literal

instance NFData Literal where
  rnf l = case l of
    IntegerLiteral i  -> rnf i
    StringLiteral s   -> rnf s
    RationalLiteral r -> rnf r

-- | Determines the Type of a Literal
literalType :: Literal
            -> Type
literalType (IntegerLiteral  _) = intPrimTy
literalType (RationalLiteral _) = voidPrimTy
literalType (StringLiteral   _) = voidPrimTy
