{-# LANGUAGE CPP #-}

module Data.List.Extra where

import Control.Applicative (liftA2)

-- | Monadic version of 'Data.List.partition'
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ []     = return ([], [])
partitionM p (x:xs) = do
  test      <- p x
  (ys, ys') <- partitionM p xs
  return $ if test then (x:ys, ys') else (ys, x:ys')

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

-- | Short-circuiting monadic version of 'any'
anyM
  :: (Monad m)
  => (a -> m Bool)
  -> [a]
  -> m Bool
anyM _ []     = return False
anyM p (x:xs) = do
  q <- p x
  if q then
    return True
  else
    anyM p xs

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ [] = return True
allM p (x:xs) = do
  q <- p x
  if q then
    allM p xs
  else
    return False

-- | short-circuiting monadic version of 'or'
orM
  :: (Monad m)
  => [m Bool]
  -> m Bool
orM [] = pure False
orM (x:xs) = do
  p <- x
  if p then
    pure True
  else
    orM xs

