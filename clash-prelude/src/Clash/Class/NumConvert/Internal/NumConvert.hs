{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK hide #-}

#include "MachDeps.h"

{- |
Copyright  :  (C) 2025     , Martijn Bastiaan
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}
module Clash.Class.NumConvert.Internal.NumConvert where

import Prelude

import Clash.Class.BitPack
import Clash.Class.Num (SaturatingNum)
import Clash.Class.NumConvert.Internal.Canonical
import Clash.Class.Resize
import Clash.Num.Erroring (Erroring, fromErroring, toErroring)
import Clash.Num.Overflowing (Overflowing, fromOverflowing, toOverflowing)
import Clash.Num.Saturating (Saturating, fromSaturating, toSaturating)
import Clash.Num.Wrapping (Wrapping, fromWrapping, toWrapping)
import Clash.Num.Zeroing (Zeroing, fromZeroing, toZeroing)
import Clash.Sized.BitVector
import Clash.Sized.Index
import Clash.Sized.Signed
import Clash.Sized.Unsigned

import GHC.TypeLits (KnownNat, type (+), type (<=), type (^))
import GHC.TypeLits.Extra (CLog)

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

{- $setup
>>> import Clash.Prelude
>>> import Clash.Class.NumConvert
>>> import Data.Word
>>> import Data.Int
-}

{- | Internal class for concrete conversions. This class is used internally by
'NumConvert' and should not be used directly. Use 'NumConvert' instead.

If you want to provide an instance for your own type, please only make an
instance for a direct mapping from and to Clash types and tell the instance
what Clash type it corresponds to. For example, implementing 'Int16' looks like:

> instance NumConvertCanonical Int16 (Signed 16) where
>   numConvertCanonical = bitCoerce
>
> instance NumConvertCanonical (Signed 16) Int16 where
>   numConvertCanonical = bitCoerce
>
> type instance Canonical Int16 = Signed 16

By doing this, your type will be convertable from and to any other type. For
example:

>>> numConvert (10 :: Int16) :: Int32
10
>>> numConvert (15 :: Signed 8) :: Int16
15
-}
class NumConvertCanonical a b where
  numConvertCanonical :: a -> b

{- | Conversions that are, based on their types, guaranteed to succeed. A
successful conversion retains the numerical value interpretation of the source
type in the target type and does not produce errors.

== __Laws__
A conversion is successful if a round trip conversion is lossless. I.e.,

> Just x == maybeNumConvert (numConvert @a @b x)

for all values @x@ of type @a@. It should also preserve the numerical value
interpretation of the bits. For types that have an @Integral@ instance, this
intuition is captured by:

> toInteger x == toInteger (numConvert @a @b x)

Instances should make sure their constraints are as \"tight\" as possible. I.e.,
if an instance's constraints cannot be satisfied, then for the same types
'Clash.Class.NumConvert.maybeNumConvert' should return 'Nothing' for one or more
values in the domain of the source type @a@:

> L.any isNothing (L.map (maybeNumConvert @a @b) [minBound ..])

All implementations should be total, i.e., they should not produce \"bottoms\".

Additionally, any implementation should be translatable to synthesizable HDL.
-}
type NumConvert a b =
  ( NumConvertCanonical a (Canonical a)
  , NumConvertCanonical (Canonical a) (Canonical b)
  , NumConvertCanonical (Canonical b) b
  )

{- | Convert a supplied value of type @a@ to a value of type @b@. The conversion
is guaranteed to succeed.

>>> numConvert (3 :: Index 8) :: Unsigned 8
3

The following will fail with a type error, as we cannot prove that all values
of @Index 8@ can be represented by an @Unsigned 2@:

>>> numConvert (3 :: Index 8) :: Unsigned 2
...

For the time being, if the input is an 'Clash.XException.XException', then
the output is too. This property might be relaxed in the future.
-}
numConvert :: forall a b. NumConvert a b => a -> b
numConvert =
    numConvertCanonical @(Canonical b) @b
  . numConvertCanonical @(Canonical a) @(Canonical b)
  . numConvertCanonical @a @(Canonical a)

instance (KnownNat n, KnownNat m, n <= m) => NumConvertCanonical (Index n) (Index m) where
  numConvertCanonical = resize

instance (KnownNat n, KnownNat m, 1 <= n, n <= 2 ^ m) => NumConvertCanonical (Index n) (Unsigned m) where
  numConvertCanonical !a = resize $ bitCoerce a

{- | Note: Conversion from @Index 1@ to @Signed 0@ is lossless, but not within the
constraints of the instance.
-}
instance (KnownNat n, KnownNat m, 1 <= n, CLog 2 n + 1 <= m) => NumConvertCanonical (Index n) (Signed m) where
  numConvertCanonical !a = numConvertCanonical $ bitCoerce @_ @(Unsigned (CLog 2 n)) a

instance (KnownNat n, KnownNat m, 1 <= n, n <= 2 ^ m) => NumConvertCanonical (Index n) (BitVector m) where
  numConvertCanonical !a = resize $ pack a

instance (KnownNat n, KnownNat m, 1 <= m, 2 ^ n <= m) => NumConvertCanonical (Unsigned n) (Index m) where
  numConvertCanonical !a = bitCoerce $ resize a

instance (KnownNat n, KnownNat m, n <= m) => NumConvertCanonical (Unsigned n) (Unsigned m) where
  numConvertCanonical = resize

{- | Note: Conversion from @Unsigned 0@ to @Signed 0@ is lossless, but not within the
constraints of the instance.
-}
instance (KnownNat n, KnownNat m, n + 1 <= m) => NumConvertCanonical (Unsigned n) (Signed m) where
  numConvertCanonical !a = bitCoerce $ resize a

instance (KnownNat n, KnownNat m, n <= m) => NumConvertCanonical (Unsigned n) (BitVector m) where
  numConvertCanonical !a = resize $ pack a

instance (KnownNat n, KnownNat m, n <= m) => NumConvertCanonical (Signed n) (Signed m) where
  numConvertCanonical !a = resize a

instance (KnownNat n, KnownNat m, 1 <= m, 2 ^ n <= m) => NumConvertCanonical (BitVector n) (Index m) where
  numConvertCanonical = unpack . resize

instance (KnownNat n, KnownNat m, n <= m) => NumConvertCanonical (BitVector n) (Unsigned m) where
  numConvertCanonical = unpack . resize

{- | Note: Conversion from @BitVector 0@ to @Signed 0@ is lossless, but not within the
constraints of the instance.
-}
instance (KnownNat n, KnownNat m, n + 1 <= m) => NumConvertCanonical (BitVector n) (Signed m) where
  numConvertCanonical = unpack . resize

instance (KnownNat n, KnownNat m, n <= m) => NumConvertCanonical (BitVector n) (BitVector m) where
  numConvertCanonical = resize

-- Concrete bidirectional instances for Word types
instance NumConvertCanonical Word (Unsigned WORD_SIZE_IN_BITS) where
  numConvertCanonical = bitCoerce
instance NumConvertCanonical (Unsigned WORD_SIZE_IN_BITS) Word where
  numConvertCanonical = bitCoerce

instance NumConvertCanonical Word64 (Unsigned 64) where
  numConvertCanonical = bitCoerce
instance NumConvertCanonical (Unsigned 64) Word64 where
  numConvertCanonical = bitCoerce

instance NumConvertCanonical Word32 (Unsigned 32) where
  numConvertCanonical = bitCoerce
instance NumConvertCanonical (Unsigned 32) Word32 where
  numConvertCanonical = bitCoerce

instance NumConvertCanonical Word16 (Unsigned 16) where
  numConvertCanonical = bitCoerce
instance NumConvertCanonical (Unsigned 16) Word16 where
  numConvertCanonical = bitCoerce

instance NumConvertCanonical Word8 (Unsigned 8) where
  numConvertCanonical = bitCoerce
instance NumConvertCanonical (Unsigned 8) Word8 where
  numConvertCanonical = bitCoerce

-- Concrete bidirectional instances for Int types
instance NumConvertCanonical Int (Signed WORD_SIZE_IN_BITS) where
  numConvertCanonical = bitCoerce
instance NumConvertCanonical (Signed WORD_SIZE_IN_BITS) Int where
  numConvertCanonical = bitCoerce

instance NumConvertCanonical Int64 (Signed 64) where
  numConvertCanonical = bitCoerce
instance NumConvertCanonical (Signed 64) Int64 where
  numConvertCanonical = bitCoerce

instance NumConvertCanonical Int32 (Signed 32) where
  numConvertCanonical = bitCoerce
instance NumConvertCanonical (Signed 32) Int32 where
  numConvertCanonical = bitCoerce

instance NumConvertCanonical Int16 (Signed 16) where
  numConvertCanonical = bitCoerce
instance NumConvertCanonical (Signed 16) Int16 where
  numConvertCanonical = bitCoerce

instance NumConvertCanonical Int8 (Signed 8) where
  numConvertCanonical = bitCoerce
instance NumConvertCanonical (Signed 8) Int8 where
  numConvertCanonical = bitCoerce

-- Concrete bidirectional instances for Bit
instance NumConvertCanonical Bit (BitVector 1) where
  numConvertCanonical = pack
instance NumConvertCanonical (BitVector 1) Bit where
  numConvertCanonical = unpack

-- Concrete bidirectional instances for wrapped number types. These are a bit
-- weird: typically we ask users to provide a mapping from their type to the
-- canonical type. Given the polymorphic nature of 'Erroring' and the link, we
-- can't do that.
instance NumConvertCanonical a b => NumConvertCanonical (Erroring a) b where
  numConvertCanonical = numConvertCanonical . fromErroring
instance (SaturatingNum a, NumConvertCanonical b a) => NumConvertCanonical b (Erroring a) where
  numConvertCanonical = toErroring . numConvertCanonical

instance NumConvertCanonical a b => NumConvertCanonical (Overflowing a) b where
  numConvertCanonical = numConvertCanonical . fromOverflowing
instance (SaturatingNum a, NumConvertCanonical b a) => NumConvertCanonical b (Overflowing a) where
  numConvertCanonical = toOverflowing . numConvertCanonical

instance NumConvertCanonical a b => NumConvertCanonical (Saturating a) b where
  numConvertCanonical = numConvertCanonical . fromSaturating
instance (SaturatingNum a, NumConvertCanonical b a) => NumConvertCanonical b (Saturating a) where
  numConvertCanonical = toSaturating . numConvertCanonical

instance NumConvertCanonical a b => NumConvertCanonical (Wrapping a) b where
  numConvertCanonical = numConvertCanonical . fromWrapping
instance (SaturatingNum a, NumConvertCanonical b a) => NumConvertCanonical b (Wrapping a) where
  numConvertCanonical = toWrapping . numConvertCanonical

instance NumConvertCanonical a b => NumConvertCanonical (Zeroing a) b where
  numConvertCanonical = numConvertCanonical . fromZeroing
instance (SaturatingNum a, NumConvertCanonical b a) => NumConvertCanonical b (Zeroing a) where
  numConvertCanonical = toZeroing . numConvertCanonical
