{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Term Literal
-}

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CLaSH.Core.Literal
  ( Literal (..)
  , literalType
  )
where

import Control.DeepSeq                        (NFData (..))
import GHC.Generics                           (Generic)
import Unbound.Generics.LocallyNameless.Extra ()
import Unbound.Generics.LocallyNameless       (Alpha (..), Subst (..))

import {-# SOURCE #-} CLaSH.Core.Type         (Type)
import CLaSH.Core.TysPrim                     (intPrimTy, integerPrimTy,
                                               charPrimTy, stringPrimTy,
                                               wordPrimTy,
                                               int64PrimTy, word64PrimTy,
                                               floatPrimTy, doublePrimTy,
                                               naturalPrimTy)

-- | Term Literal
data Literal
  = IntegerLiteral  !Integer
  | IntLiteral      !Integer
  | WordLiteral     !Integer
  | Int64Literal    !Integer
  | Word64Literal   !Integer
  | StringLiteral   !String
  | FloatLiteral    !Rational
  | DoubleLiteral   !Rational
  | CharLiteral     !Char
  | NaturalLiteral  !Integer
  deriving (Eq,Ord,Show,Generic,NFData)

instance Alpha Literal where
  fvAny' _ _ l = pure l

instance Subst a Literal where
  subst _ _ l = l
  substs _ l  = l

-- | Determines the Type of a Literal
literalType :: Literal
            -> Type
literalType (IntegerLiteral  _) = integerPrimTy
literalType (IntLiteral      _) = intPrimTy
literalType (WordLiteral     _) = wordPrimTy
literalType (StringLiteral   _) = stringPrimTy
literalType (FloatLiteral    _) = floatPrimTy
literalType (DoubleLiteral   _) = doublePrimTy
literalType (CharLiteral     _) = charPrimTy
literalType (Int64Literal    _) = int64PrimTy
literalType (Word64Literal   _) = word64PrimTy
literalType (NaturalLiteral  _) = naturalPrimTy
