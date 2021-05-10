{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021     , QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations of the Normalization process
-}

module Clash.Normalize.Transformations
  ( module X
  ) where

import Clash.Normalize.Transformations.ANF as X
import Clash.Normalize.Transformations.Case as X
import Clash.Normalize.Transformations.Cast as X
import Clash.Normalize.Transformations.DEC as X
import Clash.Normalize.Transformations.EtaExpand as X
import Clash.Normalize.Transformations.Inline as X
import Clash.Normalize.Transformations.Letrec as X
import Clash.Normalize.Transformations.MultiPrim as X
import Clash.Normalize.Transformations.Reduce as X
import Clash.Normalize.Transformations.SeparateArgs as X
import Clash.Normalize.Transformations.Specialize as X
import Clash.Normalize.Transformations.XOptimize as X
