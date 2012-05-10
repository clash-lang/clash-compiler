{-# LANGUAGE TypeOperators #-}
module CLaSH.Util
  ( module CLaSH.Util
  , mkLabels
  )
where

import Control.Monad.State              (MonadState)
import Data.Hashable                    (Hashable)
import Data.HashMap.Lazy                (HashMap)
import qualified Data.HashMap.Lazy   as HashMap
import Data.Label                       ((:->),mkLabels)
import qualified Data.Label.PureM    as LabelM
import qualified Language.Haskell.TH as TH

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
