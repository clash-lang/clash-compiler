{-|
  Copyright   :  (C) 2021     , QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities for wrapping counters consisting of multiple individual counters
-}

module Clash.Class.Counter
  ( Counter(countMin, countMax, countSuccOverflow, countPredOverflow)
  , countSucc
  , countPred
  ) where

import Clash.Class.Counter.Internal

-- $setup
-- >>> import Clash.Class.Counter
-- >>> import Clash.Sized.BitVector (BitVector)
-- >>> import Clash.Sized.Index (Index)
-- >>> import Clash.Sized.Signed (Signed)
-- >>> import Clash.Sized.Unsigned (Unsigned)

-- | Successor of a counter.
--
-- Examples:
--
-- >>> type T = (Unsigned 2, Unsigned 2)
-- >>> countSucc @T (1, 1)
-- (1,2)
-- >>> countSucc @T (1, 2)
-- (1,3)
-- >>> countSucc @T (1, 3)
-- (2,0)
-- >>> countSucc @T (3, 3)
-- (0,0)
-- >>> countSucc @(Index 9, Index 2) (0, 1)
-- (1,0)
-- >>> countSucc @(Either (Index 9) (Index 9)) (Left 8)
-- Right 0
countSucc :: Counter a => a -> a
countSucc = snd . countSuccOverflow

-- | Predecessor of a counter
--
-- Examples:
--
-- >>> type T = (Unsigned 2, Unsigned 2)
-- >>> countPred @T (1, 2)
-- (1,1)
-- >>> countPred @T (1, 3)
-- (1,2)
-- >>> countPred @T (2, 0)
-- (1,3)
-- >>> countPred @T (0, 0)
-- (3,3)
-- >>> countPred @(Index 9, Index 2) (1, 0)
-- (0,1)
-- >>> countPred @(Either (Index 9) (Index 9)) (Right 0)
-- Left 8
countPred :: Counter a => a -> a
countPred = snd . countPredOverflow
