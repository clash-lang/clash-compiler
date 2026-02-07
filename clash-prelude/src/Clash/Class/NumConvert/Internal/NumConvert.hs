{-# LANGUAGE CPP #-}
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
import Clash.Class.NumConvert.Internal.Canonical
import Clash.Class.Resize
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
-}

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
class NumConvert a b where
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
  numConvert :: a -> b

{- | Convert a value by explicitly going through the canonical "unwrapped" form
of the source type. This function is useful when direct 'numConvert' fails due
to overlapping instances.

For example, converting @Word32@ to @Word64@ creates overlapping instances, but
'numConvertVia' resolves this by explicitly routing through @Unsigned 32@:

>>> numConvertVia (42 :: Word32) :: Word64
42

The conversion path is: @Word32 -> Unsigned 32 -> Unsigned 64 -> Word64@
-}
numConvertVia ::
  forall a b.
  ( NumConvert a (Canonical a)
  , NumConvert (Canonical a) (Canonical b)
  , NumConvert (Canonical b) b
  ) =>
  a ->
  b
numConvertVia =
    numConvert @(Canonical b) @b
  . numConvert @(Canonical a) @(Canonical b)
  . numConvert @a @(Canonical a)

instance (KnownNat n, KnownNat m, n <= m) => NumConvert (Index n) (Index m) where
  numConvert = resize

instance (KnownNat n, KnownNat m, 1 <= n, n <= 2 ^ m) => NumConvert (Index n) (Unsigned m) where
  numConvert !a = resize $ bitCoerce a

{- | Note: Conversion from @Index 1@ to @Signed 0@ is lossless, but not within the
constraints of the instance.
-}
instance (KnownNat n, KnownNat m, 1 <= n, CLog 2 n + 1 <= m) => NumConvert (Index n) (Signed m) where
  numConvert !a = numConvert $ bitCoerce @_ @(Unsigned (CLog 2 n)) a

instance (KnownNat n, KnownNat m, 1 <= n, n <= 2 ^ m) => NumConvert (Index n) (BitVector m) where
  numConvert !a = resize $ pack a

instance (KnownNat n, KnownNat m, 1 <= m, 2 ^ n <= m) => NumConvert (Unsigned n) (Index m) where
  numConvert !a = bitCoerce $ resize a

instance (KnownNat n, KnownNat m, n <= m) => NumConvert (Unsigned n) (Unsigned m) where
  numConvert = resize

{- | Note: Conversion from @Unsigned 0@ to @Signed 0@ is lossless, but not within the
constraints of the instance.
-}
instance (KnownNat n, KnownNat m, n + 1 <= m) => NumConvert (Unsigned n) (Signed m) where
  numConvert !a = bitCoerce $ resize a

instance (KnownNat n, KnownNat m, n <= m) => NumConvert (Unsigned n) (BitVector m) where
  numConvert !a = resize $ pack a

instance (KnownNat n, KnownNat m, n <= m) => NumConvert (Signed n) (Signed m) where
  numConvert !a = resize a

instance (KnownNat n, KnownNat m, 1 <= m, 2 ^ n <= m) => NumConvert (BitVector n) (Index m) where
  numConvert = unpack . resize

instance (KnownNat n, KnownNat m, n <= m) => NumConvert (BitVector n) (Unsigned m) where
  numConvert = unpack . resize

{- | Note: Conversion from @BitVector 0@ to @Signed 0@ is lossless, but not within the
constraints of the instance.
-}
instance (KnownNat n, KnownNat m, n + 1 <= m) => NumConvert (BitVector n) (Signed m) where
  numConvert = unpack . resize

instance (KnownNat n, KnownNat m, n <= m) => NumConvert (BitVector n) (BitVector m) where
  numConvert = resize

-- Concrete bidirectional instances for Word types
instance NumConvert Word (Unsigned WORD_SIZE_IN_BITS) where
  numConvert = bitCoerce
instance NumConvert (Unsigned WORD_SIZE_IN_BITS) Word where
  numConvert = bitCoerce

instance NumConvert Word64 (Unsigned 64) where
  numConvert = bitCoerce
instance NumConvert (Unsigned 64) Word64 where
  numConvert = bitCoerce

instance NumConvert Word32 (Unsigned 32) where
  numConvert = bitCoerce
instance NumConvert (Unsigned 32) Word32 where
  numConvert = bitCoerce

instance NumConvert Word16 (Unsigned 16) where
  numConvert = bitCoerce
instance NumConvert (Unsigned 16) Word16 where
  numConvert = bitCoerce

instance NumConvert Word8 (Unsigned 8) where
  numConvert = bitCoerce
instance NumConvert (Unsigned 8) Word8 where
  numConvert = bitCoerce

-- Concrete bidirectional instances for Int types
instance NumConvert Int (Signed WORD_SIZE_IN_BITS) where
  numConvert = bitCoerce
instance NumConvert (Signed WORD_SIZE_IN_BITS) Int where
  numConvert = bitCoerce

instance NumConvert Int64 (Signed 64) where
  numConvert = bitCoerce
instance NumConvert (Signed 64) Int64 where
  numConvert = bitCoerce

instance NumConvert Int32 (Signed 32) where
  numConvert = bitCoerce
instance NumConvert (Signed 32) Int32 where
  numConvert = bitCoerce

instance NumConvert Int16 (Signed 16) where
  numConvert = bitCoerce
instance NumConvert (Signed 16) Int16 where
  numConvert = bitCoerce

instance NumConvert Int8 (Signed 8) where
  numConvert = bitCoerce
instance NumConvert (Signed 8) Int8 where
  numConvert = bitCoerce

-- Concrete bidirectional instances for Bit
instance NumConvert Bit (BitVector 1) where
  numConvert = pack
instance NumConvert (BitVector 1) Bit where
  numConvert = unpack
