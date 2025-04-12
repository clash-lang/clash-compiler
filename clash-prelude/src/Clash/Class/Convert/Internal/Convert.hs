{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK hide #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Class.Convert.Internal.Convert where

import Prelude

import Clash.Class.BitPack
import Clash.Class.Resize
import Clash.Sized.BitVector
import Clash.Sized.Index
import Clash.Sized.Signed
import Clash.Sized.Unsigned

import GHC.TypeLits.Extra (CLog)
import GHC.TypeLits (KnownNat, type (<=), type (+), type (^))

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

{- $setup
>>> import Clash.Prelude
>>> import Clash.Class.Convert
-}

{- | Conversions that are, based on their types, guaranteed to succeed.

== __Laws__
A conversion is safe and total if a round trip conversion is guaranteed to be
lossless. I.e.,

> Just x == maybeConvert (convert @a @b x)

for all values @x@ of type @a@. It should also preserve the numerical value
interpretation of the bits. For types that have an "Integral" instance, this
intuition is captured by:

> toInteger x == toInteger (convert @a @b x)

Instances should make sure their constraints are as \"tight\" as possible. I.e.,
if an instance exist, but the constraints cannot be satisfied, then
'Clash.Class.Convert.convertMaybe' should return 'Nothing' for one or more values in
the domain of the source type @a@:

> L.any isNothing (L.map (maybeConvert @a @b) [minBound ..])

Additionally, any implementation should be translatable to synthesizable RTL.
-}
class Convert a b where
  {- | Convert a supplied value of type @a@ to a value of type @b@. The conversion
  is guaranteed to succeed.

  >>> convert (3 :: Index 8) :: Unsigned 8
  3

  The following will fail with a type error, as we cannot prove that all values
  of @Index 8@ can be represented by an @Unsigned 2@:

  >>> convert (3 :: Index 8) :: Unsigned 2
  ...

  For the time being, if the input is an @XException@, then the output is too. This
  property might be relaxed in the future.
  -}
  convert :: a -> b

instance (KnownNat n, KnownNat m, n <= m) => Convert (Index n) (Index m) where
  convert = resize

instance (KnownNat n, KnownNat m, 1 <= n, n <= 2 ^ m) => Convert (Index n) (Unsigned m) where
  convert !a = resize $ bitCoerce a

{- | Note: Conversion from @Index 1@ to @Signed 0@ is total, but not within the
constraints of the instance.
-}
instance (KnownNat n, KnownNat m, 1 <= n, CLog 2 n + 1 <= m) => Convert (Index n) (Signed m) where
  convert !a = convert $ bitCoerce @_ @(Unsigned (CLog 2 n)) a

instance (KnownNat n, KnownNat m, 1 <= n, n <= 2 ^ m) => Convert (Index n) (BitVector m) where
  convert !a = resize $ pack a

instance (KnownNat n, KnownNat m, 1 <= m, 2 ^ n <= m) => Convert (Unsigned n) (Index m) where
  convert !a = bitCoerce $ resize a

instance (KnownNat n, KnownNat m, n <= m) => Convert (Unsigned n) (Unsigned m) where
  convert = resize

{- | Note: Conversion from @Unsigned 0@ to @Signed 0@ is total, but not within the
constraints of the instance.
-}
instance (KnownNat n, KnownNat m, n + 1 <= m) => Convert (Unsigned n) (Signed m) where
  convert = bitCoerce . resize

instance (KnownNat n, KnownNat m, n <= m) => Convert (Unsigned n) (BitVector m) where
  convert !a = resize $ pack a

instance (KnownNat n, KnownNat m, n <= m) => Convert (Signed n) (Signed m) where
  convert !a = resize a

instance (KnownNat n, KnownNat m, 1 <= m, 2 ^ n <= m) => Convert (BitVector n) (Index m) where
  convert = unpack . resize

instance (KnownNat n, KnownNat m, n <= m) => Convert (BitVector n) (Unsigned m) where
  convert = unpack . resize

{- | Note: Conversion from @BitVector 0@ to @Signed 0@ is total, but not within the
constraints of the instance.
-}
instance (KnownNat n, KnownNat m, n + 1 <= m) => Convert (BitVector n) (Signed m) where
  convert = unpack . resize

instance (KnownNat n, KnownNat m, n <= m) => Convert (BitVector n) (BitVector m) where
  convert = resize

instance (Convert (Unsigned 64) a) => Convert Word a where
  convert = convert . bitCoerce @_ @(Unsigned 64)
instance (Convert (Unsigned 64) a) => Convert Word64 a where
  convert = convert . bitCoerce @_ @(Unsigned 64)
instance (Convert (Unsigned 32) a) => Convert Word32 a where
  convert = convert . bitCoerce @_ @(Unsigned 32)
instance (Convert (Unsigned 16) a) => Convert Word16 a where
  convert = convert . bitCoerce @_ @(Unsigned 16)
instance (Convert (Unsigned 8) a) => Convert Word8 a where
  convert = convert . bitCoerce @_ @(Unsigned 8)

instance (Convert (Signed 64) a) => Convert Int a where
  convert = convert . bitCoerce @_ @(Signed 64)
instance (Convert (Signed 64) a) => Convert Int64 a where
  convert = convert . bitCoerce @_ @(Signed 64)
instance (Convert (Signed 32) a) => Convert Int32 a where
  convert = convert . bitCoerce @_ @(Signed 32)
instance (Convert (Signed 16) a) => Convert Int16 a where
  convert = convert . bitCoerce @_ @(Signed 16)
instance (Convert (Signed 8) a) => Convert Int8 a where
  convert = convert . bitCoerce @_ @(Signed 8)

instance (Convert a (Unsigned 64)) => Convert a Word where
  convert = bitCoerce @(Unsigned 64) . convert
instance (Convert a (Unsigned 64)) => Convert a Word64 where
  convert = bitCoerce @(Unsigned 64) . convert
instance (Convert a (Unsigned 32)) => Convert a Word32 where
  convert = bitCoerce @(Unsigned 32) . convert
instance (Convert a (Unsigned 16)) => Convert a Word16 where
  convert = bitCoerce @(Unsigned 16) . convert
instance (Convert a (Unsigned 8)) => Convert a Word8 where
  convert = bitCoerce @(Unsigned 8) . convert

instance (Convert a (Signed 64)) => Convert a Int where
  convert = bitCoerce @(Signed 64) . convert
instance (Convert a (Signed 64)) => Convert a Int64 where
  convert = bitCoerce @(Signed 64) . convert
instance (Convert a (Signed 32)) => Convert a Int32 where
  convert = bitCoerce @(Signed 32) . convert
instance (Convert a (Signed 16)) => Convert a Int16 where
  convert = bitCoerce @(Signed 16) . convert
instance (Convert a (Signed 8)) => Convert a Int8 where
  convert = bitCoerce @(Signed 8) . convert
