{-
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Clash.Num.Erroring
  ( Erroring(fromErroring)
  , toErroring
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
import Clash.Class.Num (SaturationMode(SatError), SaturatingNum(..))
import Clash.Class.Parity (Parity)
import Clash.Class.Resize (Resize(..))
import Clash.XException (NFDataX, ShowX, errorX)

-- | An erroring number type is one where all operations return a
-- 'Clash.XException.XExecption' if they would go out of bounds for the
-- underlying type.
--
-- Numbers can be converted to error by default using 'toErroring'.
--
newtype Erroring a =
  Erroring { fromErroring :: a }
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

{-# INLINE toErroring #-}
toErroring :: (SaturatingNum a) => a -> Erroring a
toErroring = Erroring

instance (Resize f) => Resize (Compose Erroring f) where
  {-# INLINE resize #-}
  resize
    :: forall a b
     . (KnownNat a, KnownNat b)
    => Compose Erroring f a
    -> Compose Erroring f b
  resize = coerce (resize @f @a @b)

  {-# INLINE zeroExtend #-}
  zeroExtend
    :: forall a b
     . (KnownNat a, KnownNat b)
    => Compose Erroring f a
    -> Compose Erroring f (b + a)
  zeroExtend = coerce (zeroExtend @f @a @b)

  {-# INLINE truncateB #-}
  truncateB
    :: forall a b
     . (KnownNat a)
    => Compose Erroring f (a + b)
    -> Compose Erroring f a
  truncateB = coerce (truncateB @f @a @b)

instance (Bounded a, Ord a, SaturatingNum a) => Num (Erroring a) where
  {-# INLINE (+) #-}
  (+) = coerce (satAdd @a SatError)

  {-# INLINE (-) #-}
  (-) = coerce (satSub @a SatError)

  {-# INLINE (*) #-}
  (*) = coerce (satMul @a SatError)

  negate x
    | 0 == x = x
    | 0 <= minBound @a = errorX "Erroring.negate: result exceeds minBound"
    | x == minBound = errorX "Erroring.negate: result exceeds maxBound"
    | otherwise = coerce (negate @a) x

  abs x
    | x == minBound && x < 0 = errorX "Erroring.abs: result exceeds maxBound"
    | otherwise = coerce (abs @a) x

  {-# INLINE signum #-}
  signum = coerce (signum @a)

  {-# INLINE fromInteger #-}
  -- TODO This does what the underlying representation does if the Integer
  -- is not in range (typically wrapping). It would be better if this also
  -- threw an XException, but in a way which remained synthesizable.
  fromInteger = coerce (fromInteger @a)

instance (Enum a, SaturatingNum a) => Enum (Erroring a) where
  {-# INLINE succ #-}
  succ = coerce (satSucc @a SatError)

  {-# INLINE pred #-}
  pred = coerce (satPred @a SatError)

  {-# INLINE toEnum #-}
  toEnum = coerce (toEnum @a)

  {-# INLINE fromEnum #-}
  fromEnum = coerce (fromEnum @a)

instance (Real a, SaturatingNum a) => Real (Erroring a) where
  {-# INLINE toRational #-}
  toRational = coerce (toRational @a)

instance (Integral a, SaturatingNum a) => Integral (Erroring a) where
  quotRem x y
    | x == minBound && y < 0 && y == -1 =
        (errorX "Erroring.quotRem: result exceeds maxBound", 0)
    | otherwise = coerce (quotRem @a) x y

  divMod x y
    | x == minBound && y < 0 && y == -1 =
        (errorX "Erroring.divMod: result exceeds maxBound", 0)
    | otherwise = coerce (divMod @a) x y

  {-# INLINE toInteger #-}
  toInteger = coerce (toInteger @a)

instance (Fractional a, Ord a, SaturatingNum a) => Fractional (Erroring a) where
  {-# INLINE recip #-}
  recip = coerce (recip @a)

  {-# INLINE fromRational #-}
  -- TODO This does what the underlying representation does if the Rational
  -- is not in range (typically wrapping). It would be better if this also
  -- threw an XException, but in a way which remained synthesizable.
  fromRational = coerce (fromRational @a)

instance (RealFrac a, SaturatingNum a) => RealFrac (Erroring a) where
  {-# INLINE properFraction #-}
  properFraction :: forall b. (Integral b) => Erroring a -> (b, Erroring a)
  properFraction = coerce (properFraction @a @b)
