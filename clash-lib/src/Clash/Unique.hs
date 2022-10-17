{-# LANGUAGE TypeSynonymInstances #-}

module Clash.Unique
  ( Unique
  , Uniquable (..)
  ) where

type Unique = Int

class Uniquable a where
  getUnique :: a -> Unique
  setUnique :: a -> Unique -> a

instance Uniquable Unique where
  getUnique = id
  setUnique = flip const
