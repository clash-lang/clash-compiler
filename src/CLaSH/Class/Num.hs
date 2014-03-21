{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module CLaSH.Class.Num where

import GHC.TypeLits

class Add a b where
  type AResult a b
  plus  :: a -> b -> AResult a b
  minus :: a -> b -> AResult a b

class Mult a b where
  type MResult a b
  mult :: a -> b -> MResult a b

class Resize f where
  resize :: (KnownNat a, KnownNat b) => f a -> f b
