{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Handle
  ( Handle(..)
  , HandleParent(..)
  , UnknownParent(..)
  , HandleChild(..)
  , UnknownChild(..)
  ) where

import Control.Exception (Exception)
import GHC.Stack (CallStack, prettyCallStack)

import Clash.FFI.Monad (SimCont)

class Handle a where
  nullHandle      :: a
  isNullHandle    :: a -> Bool

  freeHandle      :: a -> SimCont o ()
  compareHandles  :: a -> a -> SimCont o Bool

class Handle a => HandleParent a where
  type ParentHandle a

  parentHandle :: a -> SimCont o (ParentHandle a)

data UnknownParent a
  = UnknownParent a CallStack
  deriving anyclass (Exception)

instance (Show a) => Show (UnknownParent a) where
  show (UnknownParent a c) =
    mconcat
      [ "Unknown parent for handle "
      , show a
      , "\n"
      , prettyCallStack c
      ]

class Handle a => HandleChild i a where
  type ChildHandle i a

  childHandle :: i -> a -> SimCont o (ChildHandle i a)

data UnknownChild i a
  = UnknownChild i a CallStack
  deriving anyclass (Exception)

instance (Show i, Show a) => Show (UnknownChild i a) where
  show (UnknownChild i a c) =
    mconcat
      [ "Unknown child "
      , show i
      , " for handle "
      , show a
      , "\n"
      , prettyCallStack c
      ]

