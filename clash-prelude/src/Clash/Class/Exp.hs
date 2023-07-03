{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.Class.Exp (Exp, ExpResult, (^)) where

import qualified Prelude                       as P
import           Prelude                       hiding ((^))

import           Clash.Annotations.Primitive   (hasBlackBox)
import           Clash.Promoted.Nat            (SNat(..), snatToInteger)
import           Clash.Sized.Internal.Index    (Index)
import           Clash.Sized.Internal.Signed   (Signed)
import           Clash.Sized.Internal.Unsigned (Unsigned)

import           GHC.TypeLits
  (KnownNat, Nat, type (^), type (*))
import           GHC.TypeLits.Extra            (Max)

-- | Type class implementing exponentiation with explicitly resizing results.
class Exp a where
  type ExpResult a (n :: Nat)

  -- | Exponentiation with known exponent.
  (^)
    :: a
    -- ^ Base
    -> SNat n
    -- ^ Exponent
    -> ExpResult a n
    -- ^ Resized result, guaranteed to not have overflown

instance KnownNat m => Exp (Index m) where
  type ExpResult (Index m) n = Index (Max 2 (m ^ n))

  (^) = expIndex#
  {-# INLINE (^) #-}

instance KnownNat m => Exp (Signed m) where
  type ExpResult (Signed m) n = Signed (Max 2 (m * n))

  (^) = expSigned#
  {-# INLINE (^) #-}

instance KnownNat m => Exp (Unsigned m) where
  type ExpResult (Unsigned m) n = Unsigned (Max 1 (m * n))

  (^) = expUnsigned#
  {-# INLINE (^) #-}

expIndex#
  :: KnownNat m
  => Index m
  -> SNat n
  -> Index (Max 2 (m ^ n))
expIndex# b e@SNat =
  fromInteger (toInteger b P.^ snatToInteger e)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE expIndex# #-}
{-# ANN expIndex# hasBlackBox #-}

expSigned#
  :: KnownNat m
  => Signed m
  -> SNat n
  -> Signed (Max 2 (m * n))
expSigned# b e@SNat =
  fromInteger (toInteger b P.^ snatToInteger e)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE expSigned# #-}
{-# ANN expSigned# hasBlackBox #-}

expUnsigned#
  :: KnownNat m
  => Unsigned m
  -> SNat n
  -> Unsigned (Max 1 (m * n))
expUnsigned# b e@SNat =
  fromInteger (toInteger b P.^ snatToInteger e)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE expUnsigned# #-}
{-# ANN expUnsigned# hasBlackBox #-}
