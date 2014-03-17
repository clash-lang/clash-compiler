{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeFamilies           #-}
module CLaSH.Class.Num where

class Add a b where
  type AResult a b
  plus  :: a -> b -> AResult a b
  minus :: a -> b -> AResult a b

class Mult a b where
  type MResult a b
  mult :: a -> b -> MResult a b
