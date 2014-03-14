{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module CLaSH.Class.BitVector where

import CLaSH.Bit
import CLaSH.Sized.Vector
import GHC.TypeLits

-- | Convert types from and to a vector of @Bit@s
class BitVector a where
  -- | Number of 'Bit's needed to represents elements of type @a@
  type BitSize a :: Nat
  -- | Convert element of type @a@ to a 'Vec' of 'Bit's
  toBV   :: KnownNat (BitSize a) => a -> Vec (BitSize a) Bit
  -- | Convert a 'Vec' of 'Bit's to an element of type @a@
  fromBV :: KnownNat (BitSize a) => Vec (BitSize a) Bit -> a

instance BitVector Bit where
  type BitSize Bit = 1
  toBV   = (:> Nil)
  fromBV = vhead

instance BitVector Bool where
  type BitSize Bool = 1
  toBV   = (:> Nil) . toBit
    where
      toBit True  = H
      toBit False = L
  fromBV = fromBit . vhead
    where
      fromBit H = True
      fromBit L = False

instance (KnownNat (BitSize a), KnownNat (BitSize b), BitVector a, BitVector b) => BitVector (a,b) where
  type BitSize (a,b) = (BitSize a) + (BitSize b)
  toBV (a,b) = toBV a <++> toBV b
  fromBV bv  = (fromBV (vtakeI bv), fromBV (vdropI bv))

instance (KnownNat n, KnownNat (BitSize a), BitVector a) => BitVector (Vec n a) where
  type BitSize (Vec n a) = n * (BitSize a)
  toBV   = vconcat . vmap toBV
  fromBV = vmap fromBV . vunconcatI
