{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RoleAnnotations #-}
module CLaSH.Sized.Internal.BitVector where

import GHC.TypeLits (Nat)

type role BitVector phantom
data BitVector :: Nat -> *
type Bit = BitVector 1
