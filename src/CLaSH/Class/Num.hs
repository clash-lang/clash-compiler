{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module CLaSH.Class.Num where

import GHC.TypeLits

-- | Implicitly adding or subtracting values of two different (sub-)types.
class Add a b where
  type AResult a b
  plus  :: a -> b -> AResult a b
  minus :: a -> b -> AResult a b

-- | Implicitly multiplying values of two different (sub-)types.
class Mult a b where
  type MResult a b
  mult :: a -> b -> MResult a b

-- | Coerce a value to be represented by a different number of bits
class Resize f where
  resize :: (KnownNat a, KnownNat b) => f a -> f b
