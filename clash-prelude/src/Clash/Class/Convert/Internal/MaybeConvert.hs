{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK hide #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Class.Convert.Internal.MaybeConvert where

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

{- | Conversions that may fail for some values.

== __Laws__
A conversion is safe if a round trip conversion does not produce errors (also
see "Clash.XException"). I.e.,

> x == fromMaybe x (maybeConvert @a @b x >>= maybeConvert @b @a)

for all values @x@ of type @a@. It should also preserve the numerical value
interpretation of the bits. For types that have an "Integral" instance, this
intuition is captured by:

> toInteger x == fromMaybe (toInteger x) (toInteger (convert @a @b x))

If a conversion succeeds one way, it should also succeed the other way. I.e.,

> isJust (maybeConvert @a @b x) `implies` isJust (maybeConvert @a @b x >>= maybeConvert @b @a)

A conversion should succeed if and only if the value is representable in the
target type. For types that have a "Bounded" and "Integral" instance, this
intuition is captured by:

> isJust (maybeConvert @a @b x) == (i x >= i (minBound @b) && i x <= i (maxBound @b))

where @i = toInteger@.

Additionally, any implementation should be translatable to synthesizable RTL.
-}
class MaybeConvert a b where
  {- | Convert a supplied value of type @a@ to a value of type @b@. If the value
  cannot be represented in the target type, 'Nothing' is returned.

  >>> maybeConvert (1 :: Index 8) :: Maybe (Unsigned 2)
  Just 1
  >>> maybeConvert (7 :: Index 8) :: Maybe (Unsigned 2)
  Nothing

  For the time being, if the input is an @XException@, then the output is too.
  This property might be relaxed in the future.
  -}
  maybeConvert :: a -> Maybe b

instance (KnownNat n, KnownNat m) => MaybeConvert (Index n) (Index m) where
  maybeConvert !a = maybeResize a

instance (KnownNat n, KnownNat m, 1 <= n) => MaybeConvert (Index n) (Unsigned m) where
  maybeConvert !a = maybeResize $ bitCoerce @_ @(Unsigned (CLog 2 n)) a

instance (KnownNat n, KnownNat m, 1 <= n) => MaybeConvert (Index n) (Signed m) where
  maybeConvert !a = maybeConvert $ bitCoerce @_ @(Unsigned (CLog 2 n)) a

instance (KnownNat n, KnownNat m, 1 <= n) => MaybeConvert (Index n) (BitVector m) where
  maybeConvert !a = maybeResize $ pack a

instance (KnownNat n, KnownNat m) => MaybeConvert (Unsigned n) (Index m) where
  maybeConvert !a = maybeResize $ bitCoerce @_ @(Index (2 ^ n)) a

instance (KnownNat n, KnownNat m) => MaybeConvert (Unsigned n) (Unsigned m) where
  maybeConvert !a = maybeResize a

instance (KnownNat n, KnownNat m) => MaybeConvert (Unsigned n) (Signed m) where
  maybeConvert !a = maybeResize $ bitCoerce @(Unsigned (n + 1)) $ extend a

instance (KnownNat n, KnownNat m) => MaybeConvert (Unsigned n) (BitVector m) where
  maybeConvert !a = maybeResize $ pack a

instance (KnownNat n, KnownNat m) => MaybeConvert (Signed n) (Index m) where
  maybeConvert n
    | n < 0 = Nothing
    | otherwise = maybeResize (bitCoerce @_ @(Index (2 ^ (n + 1))) (extend n))

instance (KnownNat n, KnownNat m) => MaybeConvert (Signed n) (Unsigned m) where
  maybeConvert n
    | n < 0 = Nothing
    | otherwise = maybeResize (bitCoerce @(Signed (n + 1)) (extend n))

instance (KnownNat n, KnownNat m) => MaybeConvert (Signed n) (Signed m) where
  maybeConvert !a = maybeResize a

instance (KnownNat n, KnownNat m) => MaybeConvert (Signed n) (BitVector m) where
  maybeConvert n
    | n < 0 = Nothing
    | otherwise = maybeResize (pack @(Signed (n + 1)) (extend n))

instance (KnownNat n, KnownNat m) => MaybeConvert (BitVector n) (Index m) where
  maybeConvert !a = maybeResize $ unpack @(Index (2 ^ n)) a

instance (KnownNat n, KnownNat m) => MaybeConvert (BitVector n) (Unsigned m) where
  maybeConvert !a = maybeResize $ unpack @(Unsigned n) a

instance (KnownNat n, KnownNat m) => MaybeConvert (BitVector n) (Signed m) where
  maybeConvert !a = maybeResize $ unpack @(Signed (n + 1)) $ extend a

instance (KnownNat n, KnownNat m) => MaybeConvert (BitVector n) (BitVector m) where
  maybeConvert !a = maybeResize a

instance (MaybeConvert (Unsigned 64) a) => MaybeConvert Word a where
  maybeConvert = maybeConvert . bitCoerce @_ @(Unsigned 64)
instance (MaybeConvert (Unsigned 64) a) => MaybeConvert Word64 a where
  maybeConvert = maybeConvert . bitCoerce @_ @(Unsigned 64)
instance (MaybeConvert (Unsigned 32) a) => MaybeConvert Word32 a where
  maybeConvert = maybeConvert . bitCoerce @_ @(Unsigned 32)
instance (MaybeConvert (Unsigned 16) a) => MaybeConvert Word16 a where
  maybeConvert = maybeConvert . bitCoerce @_ @(Unsigned 16)
instance (MaybeConvert (Unsigned 8) a) => MaybeConvert Word8 a where
  maybeConvert = maybeConvert . bitCoerce @_ @(Unsigned 8)

instance (MaybeConvert (Signed 64) a) => MaybeConvert Int a where
  maybeConvert = maybeConvert . bitCoerce @_ @(Signed 64)
instance (MaybeConvert (Signed 64) a) => MaybeConvert Int64 a where
  maybeConvert = maybeConvert . bitCoerce @_ @(Signed 64)
instance (MaybeConvert (Signed 32) a) => MaybeConvert Int32 a where
  maybeConvert = maybeConvert . bitCoerce @_ @(Signed 32)
instance (MaybeConvert (Signed 16) a) => MaybeConvert Int16 a where
  maybeConvert = maybeConvert . bitCoerce @_ @(Signed 16)
instance (MaybeConvert (Signed 8) a) => MaybeConvert Int8 a where
  maybeConvert = maybeConvert . bitCoerce @_ @(Signed 8)

instance (MaybeConvert a (Unsigned 64)) => MaybeConvert a Word where
  maybeConvert = fmap (bitCoerce @(Unsigned 64)) . maybeConvert
instance (MaybeConvert a (Unsigned 64)) => MaybeConvert a Word64 where
  maybeConvert = fmap (bitCoerce @(Unsigned 64)) . maybeConvert
instance (MaybeConvert a (Unsigned 32)) => MaybeConvert a Word32 where
  maybeConvert = fmap (bitCoerce @(Unsigned 32)) . maybeConvert
instance (MaybeConvert a (Unsigned 16)) => MaybeConvert a Word16 where
  maybeConvert = fmap (bitCoerce @(Unsigned 16)) . maybeConvert
instance (MaybeConvert a (Unsigned 8)) => MaybeConvert a Word8 where
  maybeConvert = fmap (bitCoerce @(Unsigned 8)) . maybeConvert
