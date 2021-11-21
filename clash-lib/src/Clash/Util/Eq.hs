{-|
  Copyright  :  (C) 2021     , QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities related to the 'Eq' type class.
-}
{-# LANGUAGE MagicHash #-}

module Clash.Util.Eq
  ( fastEq
  , fastEqBy
  ) where

import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)

-- | Compare two values using pointer equality. If that fails, use 'Eq' to
-- determine equality. Note that this function will only shortcut for values
-- that are the same, but will always use 'Eq' for values that differ.
--
-- Values are evaluated to WHNF before comparison. This function can therefore
-- not be used if any of its arguments is expected to be bottom.
fastEq :: Eq a => a -> a -> Bool
fastEq = fastEqBy (==)

-- | Compare two values using pointer equality. If that fails, use given function
-- to determine equality. Note that this function will only shortcut for values
-- that are the same, but will always use the given function for values that
-- differ.
--
-- Values are evaluated to WHNF before comparison. This function can therefore
-- not be used if any of its arguments is expected to be bottom.
fastEqBy :: (a -> a -> Bool) -> a -> a -> Bool
fastEqBy f a1 a2
  | a1 `pointerEq` a2 = True
  | otherwise = f a1 a2

{-# NOINLINE pointerEq #-}
-- | Compares two values by comparing their positions on the heap. This function
-- will return 'True' if the values are the same object, 'False' otherwise. Note
-- that 'False' does *not* mean that the values are *not* the same. Values are
-- evaluated to WHNF before comparison.
--
-- Note: copied from @unordered-containers@.
pointerEq :: a -> a -> Bool
pointerEq !x !y = isTrue# (reallyUnsafePtrEquality# x y)
