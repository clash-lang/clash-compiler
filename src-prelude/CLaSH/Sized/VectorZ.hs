{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators  #-}
module CLaSH.Sized.VectorZ
  (VectorZ(..))
where

import GHC.TypeLits

data VectorZ :: Nat -> * -> * where
  Nil   :: VectorZ 0 a
  (::>) :: a -> VectorZ n a -> VectorZ (n + 1) a

infixr 5 ::>

instance Show a => Show (VectorZ n a) where
  show Nil        = ""
  show (x ::> xs) = show x ++ " " ++ show xs

vheadZ :: VectorZ (n + 1) a -> a
vheadZ (x ::> xs) = x

vtailZ :: VectorZ (n + 1) a -> VectorZ n a
vtailZ (x ::> xs) = xs

vlastZ :: VectorZ (n + 1) a -> a
vlastZ (x ::> Nil) = x
vlastZ  (_ ::> y ::> ys) = vlastZ (y ::> ys)

vinitZ :: VectorZ (n + 1) a -> VectorZ n a
vinitZ (_ ::> Nil)      = Nil
vinitZ (x ::> y ::> ys) = x ::> vinitZ (y ::> ys)

shiftIntoLZ :: a -> VectorZ (n + 1) a -> VectorZ (n + 1) a
shiftIntoLZ s xs = s ::> (vinitZ xs)

snocZ :: a -> VectorZ n a -> VectorZ (n + 1) a
snocZ s Nil        = s ::> Nil
snocZ s (x ::> xs) = x ::> (snocZ s xs)

shiftIntoRZ :: a -> VectorZ (n + 1) a -> VectorZ (n + 1) a
shiftIntoRZ s xs = snocZ s (vtailZ xs)

appendZ :: VectorZ n a -> VectorZ m a -> VectorZ (n + m) a
appendZ Nil        ys = ys
appendZ (x ::> xs) ys = x ::> (appendZ xs ys)
