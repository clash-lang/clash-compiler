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
import CLaSH.Core.TysPrim                     (intPrimTy, integerPrimTy, stringPrimTy, voidPrimTy)

-- | Term Literal
data Literal
  = IntegerLiteral  !Integer
  | IntLiteral      !Integer
  | StringLiteral   !String
  | RationalLiteral !Rational
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
literalType (IntLiteral _)      = intPrimTy
literalType (RationalLiteral _) = voidPrimTy
literalType (StringLiteral   _) = stringPrimTy
