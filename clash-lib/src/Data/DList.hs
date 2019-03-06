module Data.DList where

import qualified Data.List as List

newtype DList a = DL {unDL :: [a] -> [a]}

instance Show a => Show (DList a) where
  show = show . toList

instance Foldable DList where
  foldMap f   = foldMap f . toList
  {-# INLINE foldMap #-}

  foldr f x   = List.foldr f x . toList
  {-# INLINE foldr #-}

  foldl f x   = List.foldl f x . toList
  {-# INLINE foldl #-}

  foldr1 f    = List.foldr1 f . toList
  {-# INLINE foldr1 #-}

  foldl1 f    = List.foldl1 f . toList
  {-# INLINE foldl1 #-}

toList :: DList a -> [a]
toList (DL f) = f []
{-# INLINE fromList #-}

fromList :: [a] -> DList a
fromList l = DL (l ++)
{-# INLINE toList #-}

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL (x:)
{-# INLINE singleton #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x (DL xs) = DL ((x:) . xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc (DL xs) x = DL (xs . (x:))
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append (DL xs) (DL ys) = DL (xs . ys)
{-# INLINE append #-}

concat :: [DList a] -> DList a
concat = foldr append empty
{-# INLINE concat #-}
