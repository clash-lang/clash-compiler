{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators   #-}
module CLaSH.Sized.Vector where

import GHC.TypeLits  (KnownNat, Nat, type (+))
import {-# SOURCE #-} CLaSH.Sized.Internal.BitVector (BitVector, Bit)

type role Vec nominal representational
data Vec :: Nat -> * -> *

instance (KnownNat m, (~) m ((+) n 1)) => Foldable (Vec m)

bv2v :: KnownNat n => BitVector n -> Vec n Bit
map  :: (a -> b) -> Vec n a -> Vec n b
