module Clash.FFI.VPI.Handle
  ( Handle(..)
  ) where

import Clash.FFI.Monad (SimCont)

-- | A VPI handle is a reference to some VPI object.
class Handle a where
  nullHandle      :: a
  isNullHandle    :: a -> Bool

  freeHandle      :: a -> SimCont o ()
  compareHandles  :: a -> a -> SimCont o Bool

