{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module CLaSH.Class.BitReduction where

import GHC.TypeLits (KnownNat, Nat)

class BitReduction (a :: Nat -> *) where
  reduceAnd  :: KnownNat n => a n -> a 1
  reduceOr   :: KnownNat n => a n -> a 1
  reduceXor  :: KnownNat n => a n -> a 1
