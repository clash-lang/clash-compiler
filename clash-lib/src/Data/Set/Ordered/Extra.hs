{-|
  Copyright   :  (C) 2019,2021, QBayLogic B.V
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V <devops@qbaylogic.com>

  Convenience functions for "Data.Set.Ordered" from the package
  "ordered-containers".
-}

module Data.Set.Ordered.Extra
  ( OLSet
  , toListL
  ) where

import           Data.Coerce                 (coerce)
import           Data.Foldable               (toList)
import qualified Data.Set.Ordered as OSet

type OLSet a = OSet.Bias OSet.L (OSet.OSet a)

toListL :: forall a. OLSet a -> [a]
toListL = toList . coerce @(OLSet a) @(OSet.OSet a)
