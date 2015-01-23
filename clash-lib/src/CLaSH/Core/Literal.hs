{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}

-- | Term Literal
module CLaSH.Core.Literal
  ( Literal (..)
  , literalType
  )
where

import                Control.DeepSeq
import                Unbound.LocallyNameless       as Unbound hiding (rnf)
import                Unbound.LocallyNameless.Alpha

import {-# SOURCE #-} CLaSH.Core.Term               (Term)
import {-# SOURCE #-} CLaSH.Core.Type               (Type)
import                CLaSH.Core.TysPrim            (intPrimTy, voidPrimTy)

-- | Term Literal
data Literal
  = IntegerLiteral  Integer
  | StringLiteral   String
  | RationalLiteral Rational
  deriving (Eq,Ord,Show)

Unbound.derive [''Literal]

instance Alpha Rational
instance Subst b Rational

instance Alpha Literal where
  fv' _ _ = emptyC

  acompare' _ (IntegerLiteral i) (IntegerLiteral j)   = compare i j
  acompare' _ (RationalLiteral i) (RationalLiteral j) = compare i j
  acompare' c l1                 l2                   = acompareR1 rep1 c l1 l2

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
