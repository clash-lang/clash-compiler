{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module CLaSH.Class.Num where

-- | Implicitly adding or subtracting values of two different (sub-)types.
class Add a b where
  type AResult a b
  plus  :: a -> b -> AResult a b
  minus :: a -> b -> AResult a b

-- | Implicitly multiplying values of two different (sub-)types.
class Mult a b where
  type MResult a b
  mult :: a -> b -> MResult a b
