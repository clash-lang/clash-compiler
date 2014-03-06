{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeFamilies           #-}
module CLaSH.Class.Num where

import Prelude (Integer)

class Add a b | a -> b where
  type AResult a b
  add    :: a -> b -> AResult a b
  sub    :: a -> b -> AResult a b

class Mult a b | a -> b where
  type MResult a b
  mult :: a -> b -> MResult a b

class FromInteger a where
  fromInt :: Integer -> a
