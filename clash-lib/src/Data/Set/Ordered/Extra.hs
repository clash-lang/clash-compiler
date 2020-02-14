{-|
  Copyright   :  (C) 2019, QBayLogic B.V
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V <devops@qbaylogic.com>

  Convenience functions for "Data.Set.Ordered" from the package
  "ordered-containers".
-}

module Data.Set.Ordered.Extra
  ( OLSet
  , ORSet
  , toSetL
  , toSetR
  , toListL
  , toListR
  ) where

import           Data.Coerce                 (coerce)
import           Data.Foldable               (toList)
import qualified Data.Set.Ordered as OSet

type OLSet a = OSet.Bias OSet.L (OSet.OSet a)
type ORSet a = OSet.Bias OSet.R (OSet.OSet a)

toSetL :: Ord a => [a] -> OLSet a
toSetL = coerce . OSet.fromList

toSetR :: Ord a => [a] -> ORSet a
toSetR = coerce . OSet.fromList

toListL :: forall a. OLSet a -> [a]
toListL = toList . coerce @(OLSet a) @(OSet.OSet a)

toListR :: forall a. ORSet a -> [a]
toListR = toList . coerce @(ORSet a) @(OSet.OSet a)
