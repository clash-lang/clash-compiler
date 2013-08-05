{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module CLaSH.Class.BitVector where

import CLaSH.Bit
import CLaSH.Sized.Vector
import GHC.TypeLits

class BitVector a where
  type BitSize a :: Nat
  toBV   :: SingI (BitSize a) => a -> Vec (BitSize a) Bit
  fromBV :: SingI (BitSize a) => Vec (BitSize a) Bit -> a
