{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Term Literal
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
                                               voidPrimTy, wordPrimTy)

-- | Term Literal
data Literal
  = IntegerLiteral  !Integer
  | IntLiteral      !Integer
  | WordLiteral     !Integer
  | StringLiteral   !String
  | RationalLiteral !Rational
  | CharLiteral     !Char
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
literalType (RationalLiteral _) = voidPrimTy
literalType (CharLiteral     _) = charPrimTy
