{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module CLaSH.Class.BitReduction where

import GHC.TypeLits (KnownNat, Nat)

class BitReduction (a :: Nat -> *) where
  -- | Perform an \"and\" bit reduction operation between the elements of the
  -- argument.
  reduceAnd  :: KnownNat n => a n -> a 1
  -- | Perform an \"or\" bit reduction operation between the elements of the
  -- argument.
  reduceOr   :: KnownNat n => a n -> a 1
  -- | Perform an \"xor\" bit reduction operation between the elements of the
  -- argument.
  reduceXor  :: KnownNat n => a n -> a 1
