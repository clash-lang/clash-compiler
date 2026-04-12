{-|
Copyright  :  (C) 2021-2022, QBayLogic B.V.,
                  2022     , Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# OPTIONS_GHC -Wall -Werror #-}

module T3185.TH where

import Prelude hiding (Ordering(..))
import T3185.Types (Ordering(..))

compareBasicSamples :: [(Float, Float, Ordering)]
compareBasicSamples =
  [ (1.0, 2.0, LT)
  , (1.0, 2.0, LT)
  , (1.0, 2.0, LT)
  , (1.0, 2.0, LT)
  ]
