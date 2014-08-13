{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module CLaSH.Class.Bits where

import GHC.TypeLits          (KnownNat, Nat, type (+), type (*))
import Prelude               hiding (map)

import CLaSH.Class.BitIndex  (split)
import CLaSH.Sized.BitVector (BitVector, (++#), high, low)
import CLaSH.Sized.Vector    (Vec, concatBitVector#, map, unconcatBitVector#)

-- | Convert types from and to a 'Vec'tor of 'Bit's
class Bits a where
  -- | Number of 'Bit's needed to represents elements of type @a@
  type BitSize a :: Nat
  -- | Convert element of type @a@ to a 'Vec' of 'Bit's
  pack   :: KnownNat (BitSize a) => a -> BitVector (BitSize a)
  -- | Convert a 'Vec' of 'Bit's to an element of type @a@
  unpack :: KnownNat (BitSize a) => BitVector (BitSize a) -> a

instance Bits Bool where
  type BitSize Bool = 1
  pack True  = high
  pack False = low

  unpack bv  = if bv == high then True else False

instance Bits (BitVector n) where
  type BitSize (BitVector n) = n
  pack   v = v
  unpack v = v

instance (KnownNat (BitSize a), KnownNat (BitSize b), Bits a, Bits b) => Bits (a,b) where
  type BitSize (a,b) = BitSize a + BitSize b
  pack (a,b) = pack a ++# pack b
  unpack ab  = let (a,b) = split ab in (unpack a, unpack b)

instance (KnownNat n, KnownNat (BitSize a), Bits a) => Bits (Vec n a) where
  type BitSize (Vec n a) = n * (BitSize a)
  pack   = concatBitVector# . map pack
  unpack = map unpack . unconcatBitVector#
