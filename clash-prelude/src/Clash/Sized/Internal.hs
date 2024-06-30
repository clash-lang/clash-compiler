{-|
Copyright  :  (C) 2024     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.Sized.Internal where

-- | Format a range of numbers for use in error messages
--
-- If the upper bound is below the lower bound, @"\<empty range\>"@ is returned.
-- If the bounds are equal, @"[n]"@ is returned (for bounds equal to /n/).
-- Otherwise, @formatRange n m@ returns @"[n..m]"@.
formatRange ::
  (Ord a, Show a) =>
  -- | Lower bound
  a ->
  -- | Upper bound
  a ->
  String
formatRange n m
  | m < n     = "<empty range>"
  | m == n    = '[' : shows n "]"
  | otherwise = '[' : show n ++ ".." ++ shows m "]"
