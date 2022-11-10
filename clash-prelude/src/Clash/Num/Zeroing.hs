{-
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Clash.Num.Zeroing
  ( Zeroing(fromZeroing)
  , toZeroing
  ) where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Bits (Bits, FiniteBits)
import Data.Coerce (coerce)
import Data.Functor.Compose (Compose(..))
import Data.Hashable (Hashable)
import GHC.TypeLits (KnownNat, type (+))
import Test.QuickCheck (Arbitrary)

import Clash.Class.BitPack (BitPack)
import Clash.Class.Num (SaturationMode(SatZero), SaturatingNum(..))
import Clash.Class.Parity (Parity)
import Clash.Class.Resize (Resize(..))
import Clash.XException (NFDataX, ShowX)

-- | A zeroing number type is one where all operations return zero if they go
-- out of bounds for the underlying type.
--
-- Numbers can be converted to zero by default using `toZeroing`.
--
newtype Zeroing a =
  Zeroing { fromZeroing :: a }
  deriving newtype
    ( Arbitrary
    , Binary
    , Bits
    , BitPack
    , Bounded
    , Eq
    , FiniteBits
    , Hashable
    , NFData
    , NFDataX
    , Ord
    , Parity
    , Show
    , ShowX
    )

{-# INLINE toZeroing #-}
toZeroing :: (SaturatingNum a) => a -> Zeroing a
toZeroing = Zeroing

instance (Resize f) => Resize (Compose Zeroing f) where
  {-# INLINE resize #-}
  resize
    :: forall a b
     . (KnownNat a, KnownNat b)
    => Compose Zeroing f a
    -> Compose Zeroing f b
  resize = coerce (resize @f @a @b)

  {-# INLINE zeroExtend #-}
  zeroExtend
    :: forall a b
     . (KnownNat a, KnownNat b)
    => Compose Zeroing f a
    -> Compose Zeroing f (b + a)
  zeroExtend = coerce (zeroExtend @f @a @b)

  {-# INLINE truncateB #-}
  truncateB
    :: forall a b
     . (KnownNat a)
    => Compose Zeroing f (a + b)
    -> Compose Zeroing f a
  truncateB = coerce (truncateB @f @a @b)

instance (Bounded a, Ord a, SaturatingNum a) => Num (Zeroing a) where
  {-# INLINE (+) #-}
  (+) = coerce (satAdd @a SatZero)

  {-# INLINE (-) #-}
  (-) = coerce (satSub @a SatZero)

  {-# INLINE (*) #-}
  (*) = coerce (satMul @a SatZero)

  negate x
    | 0 <= minBound @a = 0
    | x == minBound = 0
    | otherwise = coerce (negate @a) x

  abs x
    | x == minBound && x < 0 = 0
    | otherwise = coerce (abs @a) x

  {-# INLINE signum #-}
  signum = coerce (signum @a)

  {-# INLINE fromInteger #-}
  -- TODO This does what the underlying representation does if the Integer
  -- is not in range (typically wrapping). It would be better if this also
  -- returned zero, but in a way which remained synthesizable.
  fromInteger = coerce (fromInteger @a)

instance (Enum a, SaturatingNum a) => Enum (Zeroing a) where
  {-# INLINE succ #-}
  -- Deliberately breaks the Enum law that succ maxBound ~> error
  succ = coerce (satSucc @a SatZero)

  {-# INLINE pred #-}
  -- Deliberately breaks the Enum law that pred minBound ~> error
  pred = coerce (satPred @a SatZero)

  {-# INLINE toEnum #-}
  toEnum = coerce (toEnum @a)

  {-# INLINE fromEnum #-}
  fromEnum = coerce (fromEnum @a)

instance (Real a, SaturatingNum a) => Real (Zeroing a) where
  {-# INLINE toRational #-}
  toRational = coerce (toRational @a)

instance (Integral a, SaturatingNum a) => Integral (Zeroing a) where
  quotRem x y
    | x == minBound && y < 0 && y == -1 = (0, 0)
    | otherwise = coerce (quotRem @a) x y

  divMod x y
    | x == minBound && y < 0 && y == -1 = (0, 0)
    | otherwise = coerce (divMod @a) x y

  {-# INLINE toInteger #-}
  toInteger = coerce (toInteger @a)

instance (Fractional a, Ord a, SaturatingNum a) => Fractional (Zeroing a) where
  {-# INLINE recip #-}
  recip = coerce (recip @a)

  {-# INLINE fromRational #-}
  -- TODO This does what the underlying representation does if the Rational
  -- is not in range (typically wrapping). It would be better if this also
  -- returned zero, but in a way which remained synthesizable.
  fromRational = coerce (fromRational @a)

instance (RealFrac a, SaturatingNum a) => RealFrac (Zeroing a) where
  {-# INLINE properFraction #-}
  properFraction :: forall b. (Integral b) => Zeroing a -> (b, Zeroing a)
  properFraction = coerce (properFraction @a @b)
