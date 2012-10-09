{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeFamilies           #-}
module CLaSH.Class.Num where

import Prelude (Integer)

class Add a b | a -> b where
  type AResult a b
  (+)    :: a -> b -> AResult a b
  (-)    :: a -> b -> AResult a b

class Mult a b | a -> b where
  type MResult a b
  (*) :: a -> b -> MResult a b

class FromInteger a where
  fromInteger :: Integer -> a
