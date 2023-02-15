{-
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Clash.Num.Wrapping
  ( Wrapping(..)
  , toWrapping
  ) where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Bits (Bits, FiniteBits)
import Data.Coerce (coerce)
import Data.Functor.Compose (Compose(..))
import Data.Hashable (Hashable)
import GHC.TypeLits (KnownNat, type (+))

import Clash.Class.BitPack (BitPack)
import Clash.Class.Num (SaturationMode(SatWrap), SaturatingNum(..))
import Clash.Class.Parity (Parity)
import Clash.Class.Resize (Resize(..))
import Clash.XException (NFDataX, ShowX)

-- | A wrapping number type is one where all operations wrap between minBound
-- and maxBound (and vice-versa) if the result goes out of bounds for the
-- underlying type.
--
-- Numbers can be converted to wrap by default using 'toWrapping'.
--
newtype Wrapping a =
  Wrapping { fromWrapping :: a }
  deriving newtype
    ( Binary
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

{-# INLINE toWrapping #-}
toWrapping :: (SaturatingNum a) => a -> Wrapping a
toWrapping = Wrapping

instance (Resize f) => Resize (Compose Wrapping f) where
  {-# INLINE resize #-}
  resize
    :: forall a b
     . (KnownNat a, KnownNat b)
    => Compose Wrapping f a
    -> Compose Wrapping f b
  resize = coerce (resize @f @a @b)

  {-# INLINE zeroExtend #-}
  zeroExtend
    :: forall a b
     . (KnownNat a, KnownNat b)
    => Compose Wrapping f a
    -> Compose Wrapping f (b + a)
  zeroExtend = coerce (zeroExtend @f @a @b)

  {-# INLINE truncateB #-}
  truncateB
    :: forall a b
     . (KnownNat a)
    => Compose Wrapping f (a + b)
    -> Compose Wrapping f a
  truncateB = coerce (truncateB @f @a @b)

instance (SaturatingNum a) => Num (Wrapping a) where
  {-# INLINE (+) #-}
  (+) = coerce (satAdd @a SatWrap)

  {-# INLINE (-) #-}
  (-) = coerce (satSub @a SatWrap)

  {-# INLINE (*) #-}
  (*) = coerce (satMul @a SatWrap)

  -- Assume the default behaviour is to wrap anyway.

  {-# INLINE negate #-}
  negate = coerce (negate @a)

  {-# INLINE abs #-}
  abs = coerce (abs @a)

  {-# INLINE signum #-}
  signum = coerce (signum @a)

  {-# INLINE fromInteger #-}
  -- TODO This does what the underlying representation does if the Integer
  -- is not in range (typically wrapping). It would be better if this also
  -- definitely wrapped, but in a way which remained synthesizable.
  fromInteger = coerce (fromInteger @a)

instance (Enum a, SaturatingNum a) => Enum (Wrapping a) where
  {-# INLINE succ #-}
  -- Deliberately breaks the Enum law that succ maxBound ~> error
  succ = coerce (satSucc @a SatWrap)

  {-# INLINE pred #-}
  -- Deliberately breaks the Enum law that pred minBound ~> error
  pred = coerce (satPred @a SatWrap)

  {-# INLINE toEnum #-}
  toEnum = coerce (toEnum @a)

  {-# INLINE fromEnum #-}
  fromEnum = coerce (fromEnum @a)

instance (Real a, SaturatingNum a) => Real (Wrapping a) where
  {-# INLINE toRational #-}
  toRational = coerce (toRational @a)

instance (Integral a, SaturatingNum a) => Integral (Wrapping a) where
  {-# INLINE quotRem #-}
  quotRem = coerce (quotRem @a)

  {-# INLINE divMod #-}
  divMod = coerce (divMod @a)

  {-# INLINE toInteger #-}
  toInteger = coerce (toInteger @a)

instance (Fractional a, SaturatingNum a) => Fractional (Wrapping a) where
  {-# INLINE recip #-}
  recip = coerce (recip @a)

  {-# INLINE fromRational #-}
  -- TODO This does what the underlying representation does if the Rational
  -- is not in range (typically wrapping). It would be better if this also
  -- definitely wrapped, but in a way which remained synthesizable.
  fromRational = coerce (fromRational @a)

instance (RealFrac a, SaturatingNum a) => RealFrac (Wrapping a) where
  {-# INLINE properFraction #-}
  properFraction :: forall b. (Integral b) => Wrapping a -> (b, Wrapping a)
  properFraction = coerce (properFraction @a @b)
