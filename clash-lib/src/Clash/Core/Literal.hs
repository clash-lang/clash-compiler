{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016     , Myrtle Software Ltd,
                     2017     , Google Inc.,
                     2021     , QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Term Literal
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Clash.Core.Literal
  ( Literal (..)
  ) where

import Control.DeepSeq                        (NFData (..))
import Data.Binary                            (Binary)
import Data.Hashable                          (Hashable)
import Data.Primitive.ByteArray               (ByteArray)
import Data.Primitive.ByteArray.Extra         ()
import Data.Word                              (Word32, Word64)
import GHC.Generics                           (Generic)

{-
Note [Storage of floating point in Literal]
-------------------------------------------

GHC stores literals of 'Float' and 'Double' as 'Rational'. However, unlike
GHC, we also need to store transfinite "literals". We need to preserve all
information there is in a specific code word representing a floating point
value.

Storing them as 'Float' and 'Double' here introduces issues with 'Eq' and
'Hashable'. 0.0 == -0.0, and NaN compares unequal to everything including
itself.

Also unlike GHC, we already assume that 'Float' is single-precision IEEE-754,
and 'Double' is double-precision IEEE-754. So we can store them as 'Word32'
and 'Word64' and get the 'Eq' and hashing properties we require.
-}

-- | Term Literal
data Literal
  = IntegerLiteral  !Integer
  | IntLiteral      !Integer
  | WordLiteral     !Integer
  | Int64Literal    !Integer
  | Word64Literal   !Integer
#if MIN_VERSION_ghc(8,8,0)
  | Int8Literal     !Integer
  | Int16Literal    !Integer
  | Int32Literal    !Integer
  | Word8Literal    !Integer
  | Word16Literal   !Integer
  | Word32Literal   !Integer
#endif
  | StringLiteral   !String
  | FloatLiteral    !Word32
  | DoubleLiteral   !Word64
  | CharLiteral     !Char
  | NaturalLiteral  !Integer
  | ByteArrayLiteral !ByteArray
  deriving (Eq,Ord,Show,Generic,NFData,Hashable,Binary)
