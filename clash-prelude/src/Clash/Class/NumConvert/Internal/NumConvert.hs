{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-}

{- | Conversions that are, based on their types, guaranteed to succeed. A
successful conversion retains the numerical value interpretation of the source
type in the target type and does not produce errors.

== __Laws__
A conversion is successful if a round trip conversion is lossless. I.e.,

> Just x == maybeNumConvert (numConvert @a @b x)

for all values @x@ of type @a@. It should also preserve the numerical value
interpretation of the bits. For types that have an "Integral" instance, this
intuition is captured by:

> toInteger x == toInteger (numConvert @a @b x)

Instances should make sure their constraints are as \"tight\" as possible. I.e.,
if an instance's constraints cannot be satisfied, then for the same types
'Clash.Class.NumConvert.maybeConvert' should return 'Nothing' for one or more
values in the domain of the source type @a@:

> L.any isNothing (L.map (maybeNumConvert @a @b) [minBound ..])

All implementations should be total, i.e., they should not produce \"bottoms\".

Additionally, any implementation should be translatable to synthesizable HDL.
-}
class NumConvert a b where
  {- | NumConvert a supplied value of type @a@ to a value of type @b@. The conversion
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

instance (NumConvert (Unsigned WORD_SIZE_IN_BITS) a) => NumConvert Word a where
  numConvert !a = numConvert $ bitCoerce @_ @(Unsigned 64) a
instance (NumConvert (Unsigned 64) a) => NumConvert Word64 a where
  numConvert !a = numConvert $ bitCoerce @_ @(Unsigned 64) a
instance (NumConvert (Unsigned 32) a) => NumConvert Word32 a where
  numConvert !a = numConvert $ bitCoerce @_ @(Unsigned 32) a
instance (NumConvert (Unsigned 16) a) => NumConvert Word16 a where
  numConvert !a = numConvert $ bitCoerce @_ @(Unsigned 16) a
instance (NumConvert (Unsigned 8) a) => NumConvert Word8 a where
  numConvert !a = numConvert $ bitCoerce @_ @(Unsigned 8) a

instance (NumConvert (Signed WORD_SIZE_IN_BITS) a) => NumConvert Int a where
  numConvert !a = numConvert $ bitCoerce @_ @(Signed 64) a
instance (NumConvert (Signed 64) a) => NumConvert Int64 a where
  numConvert !a = numConvert $ bitCoerce @_ @(Signed 64) a
instance (NumConvert (Signed 32) a) => NumConvert Int32 a where
  numConvert !a = numConvert $ bitCoerce @_ @(Signed 32) a
instance (NumConvert (Signed 16) a) => NumConvert Int16 a where
  numConvert !a = numConvert $ bitCoerce @_ @(Signed 16) a
instance (NumConvert (Signed 8) a) => NumConvert Int8 a where
  numConvert !a = numConvert $ bitCoerce @_ @(Signed 8) a

instance (NumConvert a (Unsigned WORD_SIZE_IN_BITS)) => NumConvert a Word where
  numConvert = bitCoerce @(Unsigned 64) . numConvert
instance (NumConvert a (Unsigned 64)) => NumConvert a Word64 where
  numConvert = bitCoerce @(Unsigned 64) . numConvert
instance (NumConvert a (Unsigned 32)) => NumConvert a Word32 where
  numConvert = bitCoerce @(Unsigned 32) . numConvert
instance (NumConvert a (Unsigned 16)) => NumConvert a Word16 where
  numConvert = bitCoerce @(Unsigned 16) . numConvert
instance (NumConvert a (Unsigned 8)) => NumConvert a Word8 where
  numConvert = bitCoerce @(Unsigned 8) . numConvert

instance (NumConvert a (Signed WORD_SIZE_IN_BITS)) => NumConvert a Int where
  numConvert = bitCoerce @(Signed 64) . numConvert
instance (NumConvert a (Signed 64)) => NumConvert a Int64 where
  numConvert = bitCoerce @(Signed 64) . numConvert
instance (NumConvert a (Signed 32)) => NumConvert a Int32 where
  numConvert = bitCoerce @(Signed 32) . numConvert
instance (NumConvert a (Signed 16)) => NumConvert a Int16 where
  numConvert = bitCoerce @(Signed 16) . numConvert
instance (NumConvert a (Signed 8)) => NumConvert a Int8 where
  numConvert = bitCoerce @(Signed 8) . numConvert

instance (NumConvert a (BitVector 1)) => NumConvert a Bit where
  numConvert = unpack . numConvert
instance (NumConvert (BitVector 1) a) => NumConvert Bit a where
  numConvert = numConvert . pack
