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

-- | A VPI handle is a reference to some VPI object.
class Handle a where
  nullHandle      :: a
  isNullHandle    :: a -> Bool

  freeHandle      :: a -> SimCont o ()
  compareHandles  :: a -> a -> SimCont o Bool

-- | Some handles allow accessing parents through the @vpiParent@ method. This
-- class exposes that method to be implemented by specific objects, allowing
-- them to specify the type of the parent object when the method is used.
class Handle a => HandleParent a where
  type ParentHandle a

  parentHandle :: a -> SimCont o (ParentHandle a)

-- | When the @vpiParent@ method is used on an object at the top of the design
-- hierarchy, a null handle is returned. When this happens, we can throw an
-- exception to alert authors of FFI applications that they have potentially
-- made a mistake.
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

-- | Some handles have children which can be accessed in different ways (e.g.
-- by object type, method, name or (multi)index). This class provides a way to
-- access child objects, allowing them to specify the type of child given the
-- parent and index types.
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

