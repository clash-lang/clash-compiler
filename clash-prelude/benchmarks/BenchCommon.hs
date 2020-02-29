module BenchCommon where

apSwapAp :: (a -> a -> a) -> (a,a) -> a
apSwapAp f (a,b) = f (f a b) (f b a)
{-# INLINE apSwapAp #-}

apSwapAp2 :: (a -> a -> b) -> (b -> b -> c) -> (a,a) -> c
apSwapAp2 f g (a,b) = g (f a b) (f b a)
{-# INLINE apSwapAp2 #-}
