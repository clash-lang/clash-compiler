{-# LANGUAGE ConstraintKinds #-}
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
                  2025-2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}
module Clash.Class.NumConvert.Internal.MaybeNumConvert where

import Clash.Class.BitPack
import Clash.Class.NumConvert.Internal.NumConvert (NumConvertCanonical (..))
import Clash.Class.NumConvert.Internal.Canonical (Canonical)
import Clash.Class.Resize
import Clash.Promoted.Nat
import Clash.Sized.BitVector
import Clash.Sized.Index
import Clash.Sized.Signed
import Clash.Sized.Unsigned

import GHC.TypeLits (KnownNat, type (+), type (^))
import GHC.TypeLits.Extra (CLogWZ)

{- $setup
>>> import Clash.Prelude
>>> import Clash.Class.NumConvert
>>> import Data.Word
>>> import Data.Int
-}

{- | Internal class for concrete conversions that may fail. This class is used
internally by 'MaybeNumConvert' and should not be used directly. Use
'MaybeNumConvert' instead.
-}
class MaybeNumConvertCanonical a b where
  maybeNumConvertCanonical :: a -> Maybe b

{- | Conversions that may fail for some values. A successful conversion retains
the numerical value interpretation of the source type in the target type. A
failure is expressed by returning 'Nothing', never by an 'Clash.XException.XException'.

== __Laws__
A conversion is either successful or it fails gracefully. I.e., it does not
produce errors (also see "Clash.XException"). I.e.,

> x == fromMaybe x (maybeNumConvert @a @b x >>= maybeNumConvert @b @a)

for all values @x@ of type @a@. It should also preserve the numerical value
interpretation of the bits. For types that have an @Integral@ instance, this
intuition is captured by:

> toInteger x == fromMaybe (toInteger x) (toInteger <$> maybeNumConvert @a @b x)

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
type MaybeNumConvert a b =
  ( NumConvertCanonical a (Canonical a)
  , MaybeNumConvertCanonical (Canonical a) (Canonical b)
  , NumConvertCanonical (Canonical b) b
  )

{- | Convert a supplied value of type @a@ to a value of type @b@. If the value
cannot be represented in the target type, 'Nothing' is returned.

>>> maybeNumConvert (1 :: Index 8) :: Maybe (Unsigned 2)
Just 1
>>> maybeNumConvert (7 :: Index 8) :: Maybe (Unsigned 2)
Nothing

For the time being, if the input is an 'Clash.XException.XException', then
the output is too. This property might be relaxed in the future.
-}
maybeNumConvert :: forall a b. MaybeNumConvert a b => a -> Maybe b
maybeNumConvert a =
    fmap (numConvertCanonical @(Canonical b) @b)
  $ maybeNumConvertCanonical @(Canonical a) @(Canonical b)
  $ numConvertCanonical @a @(Canonical a) a

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (Index n) (Index m) where
  maybeNumConvertCanonical !a = case natToInteger @m of
    0 -> Nothing
    _ -> maybeResize a

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (Index n) (Unsigned m) where
  maybeNumConvertCanonical !a = maybeResize $ bitCoerce @_ @(Unsigned (CLogWZ 2 n 0)) a

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (Index n) (Signed m) where
  maybeNumConvertCanonical !a = maybeNumConvertCanonical $ bitCoerce @_ @(Unsigned (CLogWZ 2 n 0)) a

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (Index n) (BitVector m) where
  maybeNumConvertCanonical !a = maybeResize $ pack a

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (Unsigned n) (Index m) where
  maybeNumConvertCanonical !a = case natToInteger @m of
    0 -> Nothing
    _ -> maybeResize $ bitCoerce @_ @(Index (2 ^ n)) a

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (Unsigned n) (Unsigned m) where
  maybeNumConvertCanonical !a = maybeResize a

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (Unsigned n) (Signed m) where
  maybeNumConvertCanonical !a = maybeResize $ bitCoerce @(Unsigned (n + 1)) $ extend a

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (Unsigned n) (BitVector m) where
  maybeNumConvertCanonical !a = maybeResize $ pack a

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (Signed n) (Index m) where
  maybeNumConvertCanonical n = case natToInteger @m of
    0 -> Nothing
    _ | n < 0 -> Nothing
      | otherwise -> maybeResize (bitCoerce @_ @(Index (2 ^ n)) (resize n))

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (Signed n) (Unsigned m) where
  maybeNumConvertCanonical n
    | n < 0 = Nothing
    | otherwise = maybeResize (bitCoerce @(Signed (n + 1)) (extend n))

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (Signed n) (Signed m) where
  maybeNumConvertCanonical !a = maybeResize a

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (Signed n) (BitVector m) where
  maybeNumConvertCanonical n
    | n < 0 = Nothing
    | otherwise = maybeResize (pack @(Signed (n + 1)) (extend n))

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (BitVector n) (Index m) where
  maybeNumConvertCanonical !a = case natToInteger @m of
    0 -> Nothing
    _ -> maybeResize $ unpack @(Index (2 ^ n)) a

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (BitVector n) (Unsigned m) where
  maybeNumConvertCanonical !a = maybeResize $ unpack @(Unsigned n) a

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (BitVector n) (Signed m) where
  maybeNumConvertCanonical !a = maybeResize $ unpack @(Signed (n + 1)) $ extend a

instance (KnownNat n, KnownNat m) => MaybeNumConvertCanonical (BitVector n) (BitVector m) where
  maybeNumConvertCanonical !a = maybeResize a
