{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Assortment of utility function used in the CLaSH library
module CLaSH.Util
  ( module CLaSH.Util
  , module X
  , makeLenses
  )
where

import Control.Applicative            as X (Applicative,(<$>),(<*>),pure)
import Control.Arrow                  as X ((***),first,second)
import Control.Monad                  as X ((<=<),(>=>))
import Control.Monad.State            (MonadState,State,StateT,runState)
import qualified Control.Monad.State  as State
import Control.Monad.Trans.Class      (MonadTrans,lift)
import Data.Function                  as X (on)
import Data.Hashable                  (Hashable(..),hash)
import Data.HashMap.Lazy              (HashMap)
import qualified Data.HashMap.Lazy    as HashMapL
import qualified Data.HashMap.Strict  as HashMapS
import Data.Maybe                     (fromMaybe)
import Control.Lens
import Debug.Trace                    (trace)
import qualified Language.Haskell.TH  as TH
import Unbound.LocallyNameless        (Embed(..))
import Unbound.LocallyNameless.Name   (Name(..))

-- | A class that can generate unique numbers
class MonadUnique m where
  -- | Get a new unique
  getUniqueM :: m Int

instance Monad m => MonadUnique (StateT Int m) where
  getUniqueM = do
    supply <- State.get
    State.modify (+1)
    return supply

instance Hashable (Name a) where
  hashWithSalt salt (Nm _ (str,int)) = hashWithSalt salt (hashWithSalt (hash int) str)
  hashWithSalt salt (Bn _ i0 i1)     = hashWithSalt salt (hash i0 `hashWithSalt` i1)

instance (Ord a) => Ord (Embed a) where
  compare (Embed a) (Embed b) = compare a b

-- | Create a TH expression that returns the a formatted string containing the
-- name of the module 'curLoc' is spliced into, and the line where it was spliced.
curLoc :: TH.Q TH.Exp
curLoc = do
  (TH.Loc _ _ modName (startPosL,_) _) <- TH.location
  TH.litE (TH.StringL $ modName ++ "(" ++ show startPosL ++ "): ")

-- | Cache the result of a monadic action
makeCached :: (MonadState s m, Hashable k, Eq k)
           => k -- ^ The key the action is associated with
           -> Lens' s (HashMap k v) -- ^ The Lens to the HashMap that is the cache
           -> m v -- ^ The action to cache
           -> m v
makeCached key l create = do
  cache <- use l
  case HashMapL.lookup key cache of
    Just value -> return value
    Nothing -> do
      value <- create
      l %= HashMapL.insert key value
      return value

-- | Cache the result of a monadic action in a State 3 transformer layers down
makeCachedT3 :: ( MonadTrans t2, MonadTrans t1, MonadTrans t
                , Eq k, Hashable k
                , MonadState s m
                , Monad (t2 m), Monad (t1 (t2 m)), Monad (t (t1 (t2 m))))
             => k -- ^ The key the action is associated with
             -> Lens' s (HashMap k v) -- ^ The Lens to the HashMap that is the cache
             -> (t (t1 (t2 m))) v -- ^ The action to cache
             -> (t (t1 (t2 m))) v
makeCachedT3 key l create = do
  cache <- (lift . lift . lift) $ use l
  case HashMapL.lookup key cache of
    Just value -> return value
    Nothing -> do
      value <- create
      (lift . lift . lift) $ l %= HashMapL.insert key value
      return value

-- | Spine-strict cache variant of 'mkCachedT3'
makeCachedT3' :: ( MonadTrans t2, MonadTrans t1, MonadTrans t
                 , Eq k, Hashable k
                 , MonadState s m
                 , Monad (t2 m), Monad (t1 (t2 m)), Monad (t (t1 (t2 m))))
              => k
              -> Lens' s (HashMap k v)
              -> (t (t1 (t2 m))) v
              -> (t (t1 (t2 m))) v
makeCachedT3' key l create = do
  cache <- (lift . lift . lift) $ use l
  case HashMapS.lookup key cache of
    Just value -> return value
    Nothing -> do
      value <- create
      (lift . lift . lift) $ l %= HashMapS.insert key value
      return value

-- | Run a State-action using the State that is stored in a higher-layer Monad
liftState :: (MonadState s m)
          => Lens' s s' -- ^ Lens to the State in the higher-layer monad
          -> State s' a -- ^ The State-action to perform
          -> m a
liftState l m = do
  s <- use l
  let (a,s') = runState m s
  l .= s'
  return a

-- | Functorial version of 'Control.Arrow.first'
firstM :: Functor f
       => (a -> f c)
       -> (a, b)
       -> f (c, b)
firstM f (x,y) = (,y) <$> f x

-- | Functorial version of 'Control.Arrow.second'
secondM :: Functor f
        => (b -> f c)
        -> (a, b)
        -> f (a, c)
secondM f (x,y) = (x,) <$> f y

-- | Performs trace when first argument evaluates to 'True'
traceIf :: Bool -> String -> a -> a
traceIf True  msg = trace msg
traceIf False _   = id

-- | Monadic version of 'Data.List.partition'
partitionM :: Monad m
           => (a -> m Bool)
           -> [a]
           -> m ([a], [a])
partitionM _ []     = return ([], [])
partitionM p (x:xs) = do
  test      <- p x
  (ys, ys') <- partitionM p xs
  return $ if test then (x:ys, ys') else (ys, x:ys')

-- | Monadic version of 'Data.List.mapAccumL'
mapAccumLM :: (Monad m)
           => (acc -> x -> m (acc,y))
           -> acc
           -> [x]
           -> m (acc,[y])
mapAccumLM _ acc [] = return (acc,[])
mapAccumLM f acc (x:xs) = do
  (acc',y) <- f acc x
  (acc'',ys) <- mapAccumLM f acc' xs
  return (acc'',y:ys)

-- | Get the state as seen through the Lens, and modify afterwards
(<%=) :: MonadState s m
      => Lens' s a
      -> (a -> a)
      -> m a
(<%=) l modify = do
  a <- use l
  l %= modify
  return a

-- | Composition of a unary function with a binary function
dot :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dot = (.) . (.)

-- | if-then-else as a function on an argument
ifThenElse :: (a -> Bool)
           -> (a -> b)
           -> (a -> b)
           -> a
           -> b
ifThenElse t f g a = if t a then f a else g a

infixr 5 <:>
-- | Applicative version of 'GHC.Types.(:)'
(<:>) :: Applicative f
      => f a
      -> f [a]
      -> f [a]
x <:> xs = (:) <$> x <*> xs

-- | Safe indexing, returns a 'Nothing' if the index does not exist
indexMaybe :: [a]
           -> Int
           -> Maybe a
indexMaybe [] _     = Nothing
indexMaybe (x:_)  0 = Just x
indexMaybe (_:xs) n = indexMaybe xs (n-1)

-- | Unsafe indexing, return a custom error message when indexing fails
indexNote :: String
          -> [a]
          -> Int
          -> a
indexNote note = fromMaybe (error note) `dot` indexMaybe

-- | Split the second list at the length of the first list
splitAtList :: [b] -> [a] -> ([a], [a])
splitAtList [] xs         = ([], xs)
splitAtList _ xs@[]       = (xs, xs)
splitAtList (_:xs) (y:ys) = (y:ys', ys'')
    where
      (ys', ys'') = splitAtList xs ys
