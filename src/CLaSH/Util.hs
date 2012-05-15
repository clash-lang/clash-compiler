{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CLaSH.Util
  ( module CLaSH.Util
  , module Control.Arrow
  , mkLabels
  )
where

import Control.Arrow                    (first,second)
import Control.Monad.State              (MonadState)
import Control.Monad.Trans.Class        (MonadTrans,lift)
import Data.Hashable                    (Hashable(..))
import Data.HashMap.Lazy                (HashMap)
import qualified Data.HashMap.Lazy   as HashMap
import Data.Label                       ((:->),mkLabels)
import qualified Data.Label.PureM    as LabelM
import Debug.Trace                      (trace)
import qualified Language.Haskell.TH as TH
import Unbound.LocallyNameless          (Rep,Bind,Embed,compareR1,eqR1,rep1)
import Unbound.LocallyNameless.Name     (Name(..))

class MonadUnique m where
  getUniqueM :: m Int

instance Hashable (Name a) where
  hash (Nm _ (str,int)) = hashWithSalt (hash int) str
  hash (Bn _ i0 i1)     = hash i0 `hashWithSalt` i1

instance (Rep a, Ord a) => Ord (Embed a) where
  compare = compareR1 rep1

instance (Rep p, Rep t, Eq p, Eq t) => Eq (Bind p t) where
  (==) = eqR1 rep1

instance (Rep p, Rep t, Ord p, Ord t) => Ord (Bind p t) where
  compare = compareR1 rep1

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

dot :: (b -> c) -> (a0 -> a1 -> b) -> a0 -> a1 -> c
dot = (.) . (.)
