{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module CLaSH.Class.BitPack
  ( BitPack (..)
  , bitCoerce
  )
where

import GHC.TypeLits                   (KnownNat, Nat, type (+))
import Prelude                        hiding (map)

import CLaSH.Sized.BitVector          (BitVector, (++#), high, low)
import CLaSH.Sized.Internal.BitVector (split#)

-- $setup
-- >>> :set -XDataKinds
-- >>> import CLaSH.Prelude

-- | Convert to and from a 'BitVector'
class BitPack a where
  -- | Number of 'CLaSH.Sized.BitVector.Bit's needed to represents elements
  -- of type @a@
  type BitSize a :: Nat
  -- | Convert element of type @a@ to a 'BitVector'
  --
  -- >>> pack (-5 :: Signed 6)
  -- 111011
  pack   :: a -> BitVector (BitSize a)
  -- | Convert a 'BitVector' to an element of type @a@
  --
  -- >>> pack (-5 :: Signed 6)
  -- 111011
  -- >>> let x = pack (-5 :: Signed 6)
  -- >>> unpack x :: Unsigned 6
  -- 59
  -- >>> pack (59 :: Unsigned 6)
  -- 111011
  unpack :: BitVector (BitSize a) -> a

{-# INLINE bitCoerce #-}
-- | Coerce a value from one type to another through its bit representation.
--
-- >>> pack (-5 :: Signed 6)
-- 111011
-- >>> bitCoerce (-5 :: Signed 6) :: Unsigned 6
-- 59
-- >>> pack (59 :: Unsigned 6)
-- 111011
bitCoerce :: (BitPack a, BitPack b, BitSize a ~ BitSize b)
          => a
          -> b
bitCoerce = unpack . pack

instance BitPack Bool where
  type BitSize Bool = 1
  pack True  = high
  pack False = low

  unpack bv  = if bv == high then True else False

instance BitPack (BitVector n) where
  type BitSize (BitVector n) = n
  pack   v = v
  unpack v = v

instance (KnownNat (BitSize a), KnownNat (BitSize b), BitPack a, BitPack b) =>
    BitPack (a,b) where
  type BitSize (a,b) = BitSize a + BitSize b
  pack (a,b) = pack a ++# pack b
  unpack ab  = let (a,b) = split# ab in (unpack a, unpack b)
