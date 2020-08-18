{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Data.List.Extra
  ( partitionM
  , mapAccumLM
  , iterateNM
  , (<:>)
  , indexMaybe
  , splitAtList
  , equalLength
  , countEq
  , zipEqual
  , dropList

  -- * From Control.Monad.Extra
  , anyM
  , allM
  , orM

  -- * From "extra"
  , module NeilsExtra
  ) where

import "extra" Data.List.Extra as NeilsExtra
import "extra" Control.Monad.Extra (anyM, allM, orM, partitionM)

import Control.Applicative (liftA2)

-- | Monadic version of 'Data.List.mapAccumL'
mapAccumLM
  :: (Monad m)
  => (acc -> x -> m (acc,y))
  -> acc
  -> [x]
  -> m (acc,[y])
mapAccumLM _ acc [] = return (acc,[])
mapAccumLM f acc (x:xs) = do
  (acc',y) <- f acc x
  (acc'',ys) <- mapAccumLM f acc' xs
  return (acc'',y:ys)

-- | Monadic version of 'iterate'. A carbon copy ('iterateM') would not
-- terminate, hence the first argument.
iterateNM
  :: Monad m
  => Word
  -- ^ Only iterate /n/ times. Note that /n/ is the length of the resulting
  -- list, _not_ the number of times the iteration function has been invoked
  -> (a -> m a)
  -- ^ Iteration function
  -> a
  -- ^ Start value
  -> m [a]
iterateNM 0 _f _a = pure []
iterateNM limit f a = fmap (a:) (go (limit - 1) a)
 where
  go 0 _a0 = pure []
  go n a0 = do
    a1 <- f a0
    fmap (a1:) (go (n - 1) a1)

infixr 5 <:>
-- | Applicative version of 'GHC.Types.(:)'
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

-- | Safe indexing, returns a 'Nothing' if the index does not exist
indexMaybe :: [a] -> Int -> Maybe a
indexMaybe [] _     = Nothing
indexMaybe (x:_)  0 = Just x
indexMaybe (_:xs) n = indexMaybe xs (n-1)

splitAtList :: [b] -> [a] -> ([a], [a])
splitAtList [] xs         = ([], xs)
splitAtList _ xs@[]       = (xs, xs)
splitAtList (_:xs) (y:ys) = (y:ys', ys'')
    where
      (ys', ys'') = splitAtList xs ys

equalLength :: [a] -> [b] -> Bool
equalLength [] [] = True
equalLength (_:as) (_:bs) = equalLength as bs
equalLength _ _ = False

-- | Return number of occurrences of an item in a list
countEq
  :: Eq a
  => a
  --  ^ Needle
  -> [a]
  -- ^ Haystack
  -> Int
  -- ^ Times needle was found in haystack
countEq a as = length (filter (== a) as)

-- | Zip two lists of equal length
--
-- NB Errors out for a DEBUG compiler when the two lists are not of equal length
zipEqual
  :: [a] -> [b] -> [(a,b)]
#if !defined(DEBUG)
zipEqual = zip
#else
zipEqual [] [] = []
zipEqual (a:as) (b:bs) = (a,b) : zipEqual as bs
zipEqual _ _ = error "zipEqual"
#endif

dropList :: [a] -> [b] -> [b]
dropList []     bs     = bs
dropList _      []     = []
dropList (_:as) (_:bs) = dropList as bs
