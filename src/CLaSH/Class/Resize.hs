{-# LANGUAGE KindSignatures #-}
module CLaSH.Class.Resize where

import GHC.TypeLits (KnownNat, Nat)

-- | Coerce a value to be represented by a different number of bits
class Resize (f :: Nat -> *) where
  resize :: (KnownNat a, KnownNat b) => f a -> f b
