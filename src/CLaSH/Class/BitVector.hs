{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
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
