{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module CLaSH.Class.BitConvert
  (BitConvert (..))
where

import GHC.TypeLits                   (KnownNat, Nat, type (+), type (*))
import Prelude                        hiding (map)

import CLaSH.Sized.BitVector          (BitVector, (++#), high, low)
import CLaSH.Sized.Internal.BitVector (split#)
import CLaSH.Sized.Vector             (Vec, concatBitVector#, map,
                                       unconcatBitVector#)

-- | Convert to and from a 'BitVector'
class BitConvert a where
  -- | Number of 'CLaSH.Sized.BitVector.Bit's needed to represents elements
  -- of type @a@
  type BitSize a :: Nat
  -- | Convert element of type @a@ to a 'BitVector'
  pack   :: a -> BitVector (BitSize a)
  -- | Convert a 'BitVector' to an element of type @a@
  unpack :: BitVector (BitSize a) -> a

instance BitConvert Bool where
  type BitSize Bool = 1
  pack True  = high
  pack False = low

  unpack bv  = if bv == high then True else False

instance BitConvert (BitVector n) where
  type BitSize (BitVector n) = n
  pack   v = v
  unpack v = v

instance (KnownNat (BitSize a), KnownNat (BitSize b), BitConvert a, BitConvert b) =>
    BitConvert (a,b) where
  type BitSize (a,b) = BitSize a + BitSize b
  pack (a,b) = pack a ++# pack b
  unpack ab  = let (a,b) = split# ab in (unpack a, unpack b)

instance (KnownNat n, KnownNat (BitSize a), BitConvert a) => BitConvert (Vec n a) where
  type BitSize (Vec n a) = n * (BitSize a)
  pack   = concatBitVector# . map pack
  unpack = map unpack . unconcatBitVector#
