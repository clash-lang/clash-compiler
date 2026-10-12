{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The transformations of the normalizer, on scoped Clash Core: the ports of
  "Clash.Normalize.Transformations".
-}

module Clash.Normalize.Scoped.Transformations
  ( module X
  ) where

import Clash.Normalize.Scoped.Transformations.ANF as X
import Clash.Normalize.Scoped.Transformations.Case as X
import Clash.Normalize.Scoped.Transformations.Cast as X
import Clash.Normalize.Scoped.Transformations.DEC as X
import Clash.Normalize.Scoped.Transformations.EtaExpand as X
import Clash.Normalize.Scoped.Transformations.Inline as X
import Clash.Normalize.Scoped.Transformations.Letrec as X
import Clash.Normalize.Scoped.Transformations.MultiPrim as X
import Clash.Normalize.Scoped.Transformations.Reduce as X
import Clash.Normalize.Scoped.Transformations.SeparateArgs as X
import Clash.Normalize.Scoped.Transformations.Specialize as X
import Clash.Normalize.Scoped.Transformations.XOptimize as X
