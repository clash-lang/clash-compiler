{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Data.List.Extra
  ( partitionM
  , mapAccumLM
  , mapAccumRM
  , iterateNM
  , (<:>)
  , indexMaybe
  , splitAtList
  , equalLength
  , countEq
  , zipEqual
  , all2

  -- * From Control.Monad.Extra
  , anyM
  , allM
  , orM

  -- * From "extra"
  , module NeilsExtra
  ) where

import "extra" Data.List.Extra as NeilsExtra
import "extra" Control.Monad.Extra (anyM, allM, orM, partitionM)

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif

#if defined(DEBUG)
import GHC.Stack (HasCallStack)
#endif

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

-- | Monadic version of 'Data.List.mapAccumR'
mapAccumRM
  :: Monad m
  => (acc -> x -> m (acc,y))
  -> acc
  -> [x]
  -> m (acc,[y])
mapAccumRM _ acc [] = return (acc,[])
mapAccumRM f acc (x:xs) = do
  (acc1,ys) <- mapAccumRM f acc xs
  (acc2,y) <- f acc1 x
  return (acc2,y:ys)

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

-- | Like 'all', but the predicate operates over two lists. Asserts to 'False'
-- when the lists are of unequal length
all2 :: (a -> b -> Bool) -> [a] -> [b] -> Bool
all2 _ [] [] = True
all2 p (a:as) (b:bs) = p a b && all2 p as bs
all2 _ _ _ = False

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
#if !defined(DEBUG)
zipEqual
  :: [a] -> [b] -> [(a,b)]
zipEqual = zip
{-# INLINE zipEqual #-}
#else
zipEqual ::
  HasCallStack =>
  [a] -> [b] -> [(a,b)]
zipEqual = go
  where
    go [] [] = []
    go (a:as) (b:bs) = (a,b) : go as bs
    go (_:_) [] = error "zipEqual: left list is longer"
    go [] (_:_) = error "zipEqual: right list is longer"
#endif
