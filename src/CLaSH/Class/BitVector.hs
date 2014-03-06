{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module CLaSH.Class.BitVector where

import CLaSH.Bit
import CLaSH.Sized.Vector
import GHC.TypeLits

-- | Convert types from and to a vector of @Bit@s
class BitVector a where
  type BitSize a :: Nat
  toBV   :: KnownNat (BitSize a) => a -> Vec (BitSize a) Bit
  fromBV :: KnownNat (BitSize a) => Vec (BitSize a) Bit -> a
