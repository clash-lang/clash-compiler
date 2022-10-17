{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Clash.Unique
  ( Unique
  , Uniquable(..)
  , MonadUnique(..)
  ) where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as StateT

type Unique = Int

class Uniquable a where
  getUnique :: a -> Unique
  setUnique :: a -> Unique -> a

instance Uniquable Unique where
  getUnique = id
  setUnique = flip const

-- TODO This class is pretty bad and we should phase it out.
class Monad m => MonadUnique m where
  getUniqueM :: m Unique

instance Monad m => MonadUnique (StateT Unique m) where
  getUniqueM = StateT.state (\u -> (u, u + 1))
