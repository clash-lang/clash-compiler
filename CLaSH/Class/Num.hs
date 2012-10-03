{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NoImplicitPrelude #-}
module CLaSH.Class.Num where

import Prelude (Integer)

class Add a where
  type AResult  a
  (+)    :: a -> a -> AResult a
  (-)    :: a -> a -> AResult a

class Mult a where
  type MResult a
  (*) :: a -> a -> (MResult a)

class FromInteger a where
  fromInteger :: Integer -> a
