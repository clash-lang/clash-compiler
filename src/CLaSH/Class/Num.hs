{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module CLaSH.Class.Num where

-- * Arithmetic functions for arguments and results of different precision

-- | Adding or subtracting values of two different (sub-)types.
class Add a b where
  type AResult a b
  plus  :: a -> b -> AResult a b
  minus :: a -> b -> AResult a b

-- | Multiplying values of two different (sub-)types.
class Mult a b where
  type MResult a b
  mult :: a -> b -> MResult a b
