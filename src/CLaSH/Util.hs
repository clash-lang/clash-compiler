{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CLaSH.Util
  ( module CLaSH.Util
  , module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , mkLabels
  )
where

import Control.Applicative              (Applicative,(<$>),(<*>),pure)
import Control.Arrow                    (first,second)
import Control.Monad                    ((<=<),(>=>))
import Control.Monad.State              (MonadState,State,runState)
import Control.Monad.Trans.Class        (MonadTrans,lift)
import Data.Hashable                    (Hashable(..),hash)
import Data.HashMap.Lazy                (HashMap)
import qualified Data.HashMap.Lazy   as HashMap
import Data.Label                       ((:->),mkLabels)
import qualified Data.Label          as Label
import qualified Data.Label.PureM    as LabelM
import Debug.Trace                      (trace)
import qualified Language.Haskell.TH as TH
import Unbound.LocallyNameless          (Embed(..))
import Unbound.LocallyNameless.Name     (Name(..))

class MonadUnique m where
  getUniqueM :: m Int

instance Hashable (Name a) where
  hashWithSalt salt (Nm _ (str,int)) = hashWithSalt salt (hashWithSalt (hash int) str)
  hashWithSalt salt (Bn _ i0 i1)     = hashWithSalt salt (hash i0 `hashWithSalt` i1)

instance (Ord a) => Ord (Embed a) where
  compare (Embed a) (Embed b) = compare a b

curLoc ::
  TH.Q TH.Exp
curLoc = do
  (TH.Loc _ _ modName (startPosL,_) _) <- TH.location
  TH.litE (TH.StringL $ modName ++ "(" ++ show startPosL ++ "): ")

makeCached ::
  (MonadState s m, Hashable k, Eq k)
  => k
  -> s :-> (HashMap k v)
  -> m v
  -> m v
makeCached key lens create = do
  cache <- LabelM.gets lens
  case HashMap.lookup key cache of
    Just value -> return value
    Nothing -> do
      value <- create
      LabelM.modify lens (HashMap.insert key value)
      return value

makeCachedT3 ::
  ( MonadTrans t2, MonadTrans t1, MonadTrans t
  , Eq k, Hashable k
  , MonadState s m
  , Monad (t2 m), Monad (t1 (t2 m)), Monad (t (t1 (t2 m))))
  => k
  -> s :-> (HashMap k v)
  -> (t (t1 (t2 m))) v
  -> (t (t1 (t2 m))) v
makeCachedT3 key lens create = do
  cache <- (lift . lift . lift) $ LabelM.gets lens
  case HashMap.lookup key cache of
    Just value -> return value
    Nothing -> do
      value <- create
      (lift . lift . lift) $ LabelM.modify lens (HashMap.insert key value)
      return value

liftState :: (MonadState s m)
          => (s :-> s')
          -> State s' a
          -> m a
liftState lens m = do
  s <- LabelM.gets lens
  let (a,s') = runState m s
  LabelM.puts lens s'
  return a

secondM ::
  Functor f
  => (b -> f c)
  -> (a, b)
  -> f (a, c)
secondM f (x,y) = fmap ((,) x) (f y)

firstM ::
  Functor f
  => (a -> f c)
  -> (a, b)
  -> f (c, b)
firstM f (x,y) = fmap (flip (,) $ y) (f x)

traceIf :: Bool -> String -> a -> a
traceIf True  msg = trace msg
traceIf False _   = id

partitionM ::
  (Monad m)
  => (a -> m Bool)
  -> [a]
  -> m ([a], [a])
partitionM _ []     = return ([], [])
partitionM p (x:xs) = do
  test      <- p x
  (ys, ys') <- partitionM p xs
  return $ if test then (x:ys, ys') else (ys, x:ys')

mapAccumLM ::
  (Monad m)
  => (acc -> x -> m (acc,y))
  -> acc
  -> [x]
  -> m (acc,[y])
mapAccumLM _ acc [] = return (acc,[])
mapAccumLM f acc (x:xs) = do
  (acc',y) <- f acc x
  (acc'',ys) <- mapAccumLM f acc' xs
  return (acc'',y:ys)

getAndModify ::
  MonadState s m
  => (s :-> a)
  -> (a -> a)
  -> m a
getAndModify lens modify = do
  a <- LabelM.gets lens
  LabelM.modify lens modify
  return a

dot :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dot = (.) . (.)

(><) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(f >< g) (a,b) = (f a, g b)

ifThenElse ::
  (a -> Bool)
  -> (a -> b)
  -> (a -> b)
  -> a
  -> b
ifThenElse t f g a = if t a then f a else g a

infixr 5 <:>
(<:>) :: Applicative f
      => f a
      -> f [a]
      -> f [a]
x <:> xs = (:) <$> x <*> xs

__1 :: ((a,b) :-> a)
__1 = Label.lens fst (\x (_,y) -> (x,y))

__2 :: ((a,b) :-> b)
__2 = Label.lens snd (\y (x,_) -> (x,y))

