{-
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Clash.Num.Saturating
  ( Saturating
  , fromSaturating  -- exported here because haddock https://github.com/haskell/haddock/issues/456
  , toSaturating
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
import Clash.Class.Num (SaturationMode(SatBound), SaturatingNum(..))
import Clash.Class.Parity (Parity)
import Clash.Class.Resize (Resize(..))
import Clash.XException (NFDataX, ShowX)

-- | A saturating number type is one where all operations saturate at the
-- bounds of the underlying type, i.e. operations which overflow return
-- 'maxBound' and operations that underflow return 'minBound'.
--
-- Numbers can be converted to saturate by default using 'toSaturating'.
--
newtype Saturating a =
  Saturating { fromSaturating :: a }
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

{-# INLINE toSaturating #-}
toSaturating :: (SaturatingNum a) => a -> Saturating a
toSaturating = Saturating

instance (Resize f) => Resize (Compose Saturating f) where
  {-# INLINE resize #-}
  resize
    :: forall a b
     . (KnownNat a, KnownNat b)
    => Compose Saturating f a
    -> Compose Saturating f b
  resize = coerce (resize @f @a @b)

  {-# INLINE zeroExtend #-}
  zeroExtend
    :: forall a b
     . (KnownNat a, KnownNat b)
    => Compose Saturating f a
    -> Compose Saturating f (b + a)
  zeroExtend = coerce (zeroExtend @f @a @b)

  {-# INLINE truncateB #-}
  truncateB
    :: forall a b
     . (KnownNat a)
    => Compose Saturating f (a + b)
    -> Compose Saturating f a
  truncateB = coerce (truncateB @f @a @b)

instance (Ord a, SaturatingNum a) => Num (Saturating a) where
  {-# INLINE (+) #-}
  (+) = coerce (satAdd @a SatBound)

  {-# INLINE (-) #-}
  (-) = coerce (satSub @a SatBound)

  {-# INLINE (*) #-}
  (*) = coerce (satMul @a SatBound)

  negate x
    | 0 <= minBound @a = 0
    | x == minBound = maxBound
    | otherwise = coerce (negate @a) x

  abs x
    | x == minBound && x < 0 = maxBound
    | otherwise = coerce (abs @a) x

  {-# INLINE signum #-}
  signum = coerce (signum @a)

  {-# INLINE fromInteger #-}
  -- TODO This does what the underlying representation does if the Integer
  -- is not in range (typically wrapping). It would be better if this also
  -- saturated, but in a way which remained synthesizable.
  fromInteger = coerce (fromInteger @a)

instance (Enum a, SaturatingNum a) => Enum (Saturating a) where
  {-# INLINE succ #-}
  -- Deliberately breaks the Enum law that succ maxBound ~> error
  succ = coerce (satSucc @a SatBound)

  {-# INLINE pred #-}
  -- Deliberately breaks the Enum law that pred minBound ~> error
  pred = coerce (satPred @a SatBound)

  {-# INLINE toEnum #-}
  toEnum = coerce (toEnum @a)

  {-# INLINE fromEnum #-}
  fromEnum = coerce (fromEnum @a)

instance (Real a, SaturatingNum a) => Real (Saturating a) where
  {-# INLINE toRational #-}
  toRational = coerce (toRational @a)

instance (Integral a, SaturatingNum a) => Integral (Saturating a) where
  -- NOTE the seemingly duplicate "y < 0 && y == -1" guards against unsigned types
  quotRem x y
    | x == minBound && y < 0 && y == -1 = (maxBound, 0)
    | otherwise = coerce (quotRem @a) x y

  divMod x y
    | x == minBound && y < 0 && y == -1 = (maxBound, 0)
    | otherwise = coerce (divMod @a) x y

  {-# INLINE toInteger #-}
  toInteger = coerce (toInteger @a)

instance (Fractional a, Ord a, SaturatingNum a) => Fractional (Saturating a) where
  {-# INLINE recip #-}
  recip = coerce (recip @a)

  {-# INLINE fromRational #-}
  -- TODO This does what the underlying representation does if the Rational
  -- is not in range (typically wrapping). It would be better if this also
  -- saturated, but in a way which remained synthesizable.
  fromRational = coerce (fromRational @a)

instance (Ord a, RealFrac a, SaturatingNum a) => RealFrac (Saturating a) where
  {-# INLINE properFraction #-}
  properFraction :: forall b. (Integral b) => Saturating a -> (b, Saturating a)
  properFraction = coerce (properFraction @a @b)
