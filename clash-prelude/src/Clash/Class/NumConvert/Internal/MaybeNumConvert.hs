{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright  :  (C) 2025     , Martijn Bastiaan
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}
module Clash.Class.NumConvert.Internal.MaybeNumConvert where

import Clash.Class.BitPack
import Clash.Class.NumConvert.Internal.NumConvert (NumConvert (..))
import Clash.Class.NumConvert.Internal.Canonical (Canonical)
import Clash.Class.Resize
import Clash.Sized.BitVector
import Clash.Sized.Index
import Clash.Sized.Signed
import Clash.Sized.Unsigned

import GHC.TypeLits (KnownNat, type (+), type (<=), type (^))
import GHC.TypeLits.Extra (CLog)

{- $setup
>>> import Clash.Prelude
>>> import Clash.Class.NumConvert
>>> import Data.Word
>>> import Data.Int
-}

{- | Conversions that may fail for some values. A successful conversion retains
the numerical value interpretation of the source type in the target type. A
failure is expressed by returning 'Nothing', never by an 'Clash.XException.XException'.

== __Laws__
A conversion is either successful or it fails gracefully. I.e., it does not
produces produce errors (also see "Clash.XException"). I.e.,

> x == fromMaybe x (maybeNumConvert @a @b x >>= maybeNumConvert @b @a)

for all values @x@ of type @a@. It should also preserve the numerical value
interpretation of the bits. For types that have an @Integral@ instance, this
intuition is captured by:

> toInteger x == fromMaybe (toInteger x) (toInteger (numConvert @a @b x))

If a conversion succeeds one way, it should also succeed the other way. I.e.,

> isJust (maybeNumConvert @a @b x) `implies` isJust (maybeNumConvert @a @b x >>= maybeNumConvert @b @a)

A conversion should succeed if and only if the value is representable in the
target type. For types that have a @Bounded@ and @Integral@ instance, this
intuition is captured by:

> isJust (maybeNumConvert @a @b x) == (i x >= i (minBound @b) && i x <= i (maxBound @b))

where @i = toInteger@.

All implementations should be total, i.e., they should not produce \"bottoms\".

Additionally, any implementation should be translatable to synthesizable HDL.
-}
class MaybeNumConvert a b where
  {- | Convert a supplied value of type @a@ to a value of type @b@. If the value
    cannot be represented in the target type, 'Nothing' is returned.

    >>> maybeNumConvert (1 :: Index 8) :: Maybe (Unsigned 2)
    Just 1
    >>> maybeNumConvert (7 :: Index 8) :: Maybe (Unsigned 2)
    Nothing

    For the time being, if the input is an 'Clash.XException.XException', then
    the output is too. This property might be relaxed in the future.
  -}
  maybeNumConvert :: a -> Maybe b

{- | Convert a value by explicitly going through the canonical "unwrapped" form
of the source and target types. This function is useful when direct 'maybeNumConvert'
fails due to overlapping instances.

For example, converting @Int64@ to @Word64@ creates overlapping instances, but
'maybeNumConvertVia' resolves this by explicitly routing through canonical forms:

>>> maybeNumConvertVia (42 :: Int64) :: Maybe Word64
Just 42
>>> maybeNumConvertVia (-1 :: Int64) :: Maybe Word64
Nothing

The conversion uses 'NumConvert' for the conversions to/from canonical forms (which
always succeed) and 'MaybeNumConvert' for the Clash-to-Clash conversion (which may fail).
The conversion path is: @Int64 -> Signed 64 -> Unsigned 64 -> Word64@
-}
maybeNumConvertVia ::
  forall a b.
  ( NumConvert a (Canonical a)
  , MaybeNumConvert (Canonical a) (Canonical b)
  , NumConvert (Canonical b) b
  ) =>
  a ->
  Maybe b
maybeNumConvertVia a =
    fmap (numConvert @(Canonical b) @b)
  $ maybeNumConvert @(Canonical a) @(Canonical b)
  $ numConvert @a @(Canonical a) a

instance (KnownNat n, KnownNat m) => MaybeNumConvert (Index n) (Index m) where
  maybeNumConvert !a = maybeResize a

instance (KnownNat n, KnownNat m, 1 <= n) => MaybeNumConvert (Index n) (Unsigned m) where
  maybeNumConvert !a = maybeResize $ bitCoerce @_ @(Unsigned (CLog 2 n)) a

instance (KnownNat n, KnownNat m, 1 <= n) => MaybeNumConvert (Index n) (Signed m) where
  maybeNumConvert !a = maybeNumConvert $ bitCoerce @_ @(Unsigned (CLog 2 n)) a

instance (KnownNat n, KnownNat m, 1 <= n) => MaybeNumConvert (Index n) (BitVector m) where
  maybeNumConvert !a = maybeResize $ pack a

instance (KnownNat n, KnownNat m) => MaybeNumConvert (Unsigned n) (Index m) where
  maybeNumConvert !a = maybeResize $ bitCoerce @_ @(Index (2 ^ n)) a

instance (KnownNat n, KnownNat m) => MaybeNumConvert (Unsigned n) (Unsigned m) where
  maybeNumConvert !a = maybeResize a

instance (KnownNat n, KnownNat m) => MaybeNumConvert (Unsigned n) (Signed m) where
  maybeNumConvert !a = maybeResize $ bitCoerce @(Unsigned (n + 1)) $ extend a

instance (KnownNat n, KnownNat m) => MaybeNumConvert (Unsigned n) (BitVector m) where
  maybeNumConvert !a = maybeResize $ pack a

instance (KnownNat n, KnownNat m) => MaybeNumConvert (Signed n) (Index m) where
  maybeNumConvert n
    | n < 0 = Nothing
    | otherwise = maybeResize (bitCoerce @_ @(Index (2 ^ n)) (resize n))

instance (KnownNat n, KnownNat m) => MaybeNumConvert (Signed n) (Unsigned m) where
  maybeNumConvert n
    | n < 0 = Nothing
    | otherwise = maybeResize (bitCoerce @(Signed (n + 1)) (extend n))

instance (KnownNat n, KnownNat m) => MaybeNumConvert (Signed n) (Signed m) where
  maybeNumConvert !a = maybeResize a

instance (KnownNat n, KnownNat m) => MaybeNumConvert (Signed n) (BitVector m) where
  maybeNumConvert n
    | n < 0 = Nothing
    | otherwise = maybeResize (pack @(Signed (n + 1)) (extend n))

instance (KnownNat n, KnownNat m) => MaybeNumConvert (BitVector n) (Index m) where
  maybeNumConvert !a = maybeResize $ unpack @(Index (2 ^ n)) a

instance (KnownNat n, KnownNat m) => MaybeNumConvert (BitVector n) (Unsigned m) where
  maybeNumConvert !a = maybeResize $ unpack @(Unsigned n) a

instance (KnownNat n, KnownNat m) => MaybeNumConvert (BitVector n) (Signed m) where
  maybeNumConvert !a = maybeResize $ unpack @(Signed (n + 1)) $ extend a

instance (KnownNat n, KnownNat m) => MaybeNumConvert (BitVector n) (BitVector m) where
  maybeNumConvert !a = maybeResize a
