{-|
Copyright  :  (C) 2021-2025, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.Num.Overflowing
  ( Overflowing
  , fromOverflowing
  , hasOverflowed
  , toOverflowing
  , clearOverflow
  ) where

import Prelude hiding (even, odd)

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Function (on)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, type (+))

import Clash.Class.BitPack (BitPack(..))
import Clash.Class.Finite (Finite(..))
import Clash.Class.Num (SaturationMode(SatWrap, SatZero), SaturatingNum(..))
import Clash.Class.Parity (Parity(..))
import Clash.XException (NFDataX, ShowX)

-- | An overflowing number behaves similarly to a 'Clash.Num.Wrapping.Wrapping'
-- number, but also includes an overflow status flag which can be used to more
-- easily check if an overflow has occurred.
--
-- Numbers can be converted to be 'Overflowing' using 'toOverflowing'.
--
data Overflowing a = Overflowing
  { fromOverflowing :: a
    -- ^ Retrieve the value
  , hasOverflowed :: Bool
    -- ^ 'True' when a computation has overflowed
  }
  deriving stock (Generic, Show)
  deriving anyclass (Binary, Finite, Hashable, NFData, NFDataX, ShowX)

{-# INLINE toOverflowing #-}
toOverflowing :: a -> Overflowing a
toOverflowing x = Overflowing x False

{-# INLINE clearOverflow #-}
-- | Reset the overflow status flag to False.
clearOverflow :: Overflowing a -> Overflowing a
clearOverflow x = x { hasOverflowed = False }

instance (Eq a) => Eq (Overflowing a) where
  {-# INLINE (==) #-}
  (==) = (==) `on` fromOverflowing

instance (Ord a) => Ord (Overflowing a) where
  {-# INLINE compare #-}
  compare = compare `on` fromOverflowing

instance (BitPack a, KnownNat (BitSize a + 1)) => BitPack (Overflowing a) where
  type BitSize (Overflowing a) = BitSize a + 1
  -- Default instance, no explicit implementations.

instance (Parity a) => Parity (Overflowing a) where
  {-# INLINE even #-}
  even = even . fromOverflowing

  {-# INLINE odd #-}
  odd = odd . fromOverflowing

instance (Bounded a, Ord a, SaturatingNum a) => Num (Overflowing a) where
  Overflowing x a + Overflowing y b
    | y > 0
    , x > satSub SatWrap maxBound y
    = withOverflow True

    | y < 0
    , x < satSub SatWrap minBound y
    = withOverflow True

    | otherwise
    = withOverflow (a || b)
   where
    withOverflow = Overflowing (satAdd SatWrap x y)

  Overflowing x a - Overflowing y b
    | y < 0
    , x > satAdd SatWrap maxBound y
    = withOverflow True

    | y > 0
    , x < satAdd SatWrap minBound y
    = withOverflow True

    | otherwise
    = withOverflow (a || b)
   where
    withOverflow = Overflowing (satSub SatWrap x y)

  Overflowing x a * Overflowing y b
    | x /= 0
    , y /= 0
    , satMul SatZero x y == 0
    = withOverflow True

    | otherwise
    = withOverflow (a || b)
   where
    withOverflow = Overflowing (satMul SatWrap x y)

  negate n@(Overflowing x a)
    | 0 == x = n
    | 0 <= minBound @a = withOverflow True
    | x == minBound = withOverflow True
    | otherwise = withOverflow a
   where
    withOverflow = Overflowing (negate x)

  abs (Overflowing x a)
    | x == minBound && x < 0 = Overflowing x True
    | otherwise = Overflowing (abs x) a

  signum (Overflowing x a) =
    Overflowing (signum x) a

  -- TODO This does what the underlying representation does if the Integer
  -- is not in range (typically wrapping). It would be better if this also
  -- definitely wrapped, but in a way which remained synthesizable.
  fromInteger i =
    Overflowing (fromInteger i) False

instance (Bounded a) => Bounded (Overflowing a) where
  minBound = Overflowing minBound False
  maxBound = Overflowing maxBound False

instance (Enum a, Eq a, SaturatingNum a) => Enum (Overflowing a) where
  succ (Overflowing x a)
    | x == maxBound = withOverflow True
    | otherwise = withOverflow a
   where
    withOverflow = Overflowing (satSucc SatWrap x)

  pred (Overflowing x a)
    | x == minBound = withOverflow True
    | otherwise = withOverflow a
   where
    withOverflow = Overflowing (satPred SatWrap x)

  toEnum i = Overflowing (toEnum i) False
  fromEnum = fromEnum . fromOverflowing

instance (Real a, SaturatingNum a) => Real (Overflowing a) where
  toRational = toRational . fromOverflowing

instance (Integral a, SaturatingNum a) => Integral (Overflowing a) where
  -- NOTE the seemingly duplicate "y < 0 && y == -1" guards against unsigned types
  quotRem (Overflowing x a) (Overflowing y b)
    | x == minBound && y < 0 && y == -1 =
        withOverflow True

    | otherwise =
        withOverflow (a || b)
   where
    withOverflow o =
      let (q, r) = quotRem x y
       in (Overflowing q o, Overflowing r False)

  divMod (Overflowing x a) (Overflowing y b)
    | x == minBound && y < 0 && y == -1 =
        withOverflow True

    | otherwise =
        withOverflow (a || b)
   where
    withOverflow o =
      let (d, m) = divMod x y
       in (Overflowing d o, Overflowing m False)

  toInteger = toInteger . fromOverflowing

instance (Fractional a, Ord a, SaturatingNum a) => Fractional (Overflowing a) where
  recip x =
    x { fromOverflowing = recip (fromOverflowing x) }

  -- TODO This does what the underlying representation does if the Rational
  -- is not in range (typically wrapping). It would be better if this also
  -- definitely wrapped, but in a way which remained synthesizable.
  fromRational i =
    Overflowing (fromRational i) False

instance (RealFrac a, SaturatingNum a) => RealFrac (Overflowing a) where
  properFraction (Overflowing x _) =
    let (n, f) = properFraction x
     in (n, Overflowing f False)
