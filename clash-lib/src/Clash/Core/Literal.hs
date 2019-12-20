{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2017     , Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Term Literal
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Clash.Core.Literal
  ( Literal (..)
  , literalType
  )
where

import Control.DeepSeq                        (NFData (..))
import Data.Binary                            (Binary)
import Data.Vector.Binary                     ()
import Data.Hashable                          (Hashable)
import Data.Vector.Primitive.Extra            (Vector)
import Data.Word                              (Word8)
import GHC.Generics                           (Generic)

import {-# SOURCE #-} Clash.Core.Type         (Type)
import Clash.Core.TysPrim                     (intPrimTy, integerPrimTy,
                                               charPrimTy, stringPrimTy,
                                               wordPrimTy,
                                               int64PrimTy, word64PrimTy,
                                               floatPrimTy, doublePrimTy,
                                               naturalPrimTy, byteArrayPrimTy)

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
  | ByteArrayLiteral !(Vector Word8)
  deriving (Eq,Ord,Show,Generic,NFData,Hashable,Binary)

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
literalType (ByteArrayLiteral _) = byteArrayPrimTy
