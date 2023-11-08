{-|
Copyright  :  (C) 2013-2016, University of Twente
                  2020,      Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Class.Resize
 ( Resize(..)

 -- * Resize helpers
 , checkedResize
 , checkedFromIntegral
 , checkedTruncateB
 ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import GHC.Stack (HasCallStack)
import GHC.TypeLits (Nat, KnownNat, type (+))

-- | Coerce a value to be represented by a different number of bits
class Resize (f :: Nat -> Type) where
  -- | A sign-preserving resize operation
  --
  -- * For signed datatypes: Increasing the size of the number replicates the
  -- sign bit to the left. Truncating a number to length L keeps the sign bit
  -- and the rightmost L-1 bits.
  --
  -- * For unsigned datatypes: Increasing the size of the number extends with
  -- zeros to the left. Truncating a number of length N to a length L just
  -- removes the left (most significant) N-L bits.
  resize :: (KnownNat a, KnownNat b) => f a -> f b
  -- | Perform a 'zeroExtend' for unsigned datatypes, and 'signExtend' for a
  -- signed datatypes
  extend :: (KnownNat a, KnownNat b) => f a -> f (b + a)
  extend = resize
  -- | Add extra zero bits in front of the MSB
  zeroExtend :: (KnownNat a, KnownNat b) => f a -> f (b + a)
  -- | Add extra sign bits in front of the MSB
  signExtend :: (KnownNat a, KnownNat b) => f a -> f (b + a)
  signExtend = resize
  -- | Remove bits from the MSB
  truncateB :: KnownNat a => f (a + b) -> f a

-- | Helper function of 'checkedFromIntegral', 'checkedResize' and 'checkedTruncateB'
checkIntegral ::
  forall a b.
  HasCallStack =>
  (Integral a, Integral b, Bounded b) =>
  Proxy b ->
  a -> ()
checkIntegral Proxy v =
  if toInteger v > toInteger (maxBound @b)
  || toInteger v < toInteger (minBound @b) then
    error $ "Given integral " <> show (toInteger v) <> " is out of bounds for" <>
            " target type. Bounds of target type are: [" <>
            show (toInteger (minBound @b)) <> ".." <>
            show (toInteger (maxBound @b)) <> "]."
  else
    ()

-- | Like 'fromIntegral', but errors if /a/ is out of bounds for /b/. Useful when
-- you "know" /a/ can't be out of bounds, but would like to have your assumptions
-- checked.
--
-- * __NB__: Check only affects simulation. I.e., no checks will be inserted
-- into the generated HDL
-- * __NB__: 'fromIntegral' is not well suited for Clash as it will go through
-- 'Integer' which is arbitrarily bounded in HDL. Instead use
-- 'Clash.Class.BitPack.bitCoerce' and the 'Resize' class.
checkedFromIntegral ::
  forall a b.
  HasCallStack =>
  (Integral a, Integral b, Bounded b) =>
  a -> b
checkedFromIntegral v =
  checkIntegral (Proxy @b) v `seq` fromIntegral v

-- | Like 'resize', but errors if /f a/ is out of bounds for /f b/. Useful when
-- you "know" /f a/ can't be out of bounds, but would like to have your
-- assumptions checked.
--
-- __NB__: Check only affects simulation. I.e., no checks will be inserted
-- into the generated HDL
checkedResize ::
  forall a b f.
  ( HasCallStack
  , Resize f
  , KnownNat a, Integral (f a)
  , KnownNat b, Integral (f b), Bounded (f b) ) =>
  f a -> f b
checkedResize v =
  checkIntegral (Proxy @(f b)) v `seq` resize v

-- | Like 'truncateB', but errors if /f (a + b)/ is out of bounds for /f a/. Useful
-- when you "know" /f (a + b)/ can't be out of bounds, but would like to have your
-- assumptions checked.
--
-- __NB__: Check only affects simulation. I.e., no checks will be inserted
-- into the generated HDL
checkedTruncateB ::
  forall a b f.
  ( HasCallStack
  , Resize f
  , KnownNat b, Integral (f (a + b))
  , KnownNat a, Integral (f a), Bounded (f a) ) =>
  f (a + b) -> f a
checkedTruncateB v =
  checkIntegral (Proxy @(f a)) v `seq` truncateB v
