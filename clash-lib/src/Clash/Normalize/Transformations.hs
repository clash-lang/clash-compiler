{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The transformations of the normalizer, on scoped Clash Core. They are
  grouped into submodules by the kind of term they work on, or by their
  purpose: ANF conversion, case-expressions, casts, disjoint expression
  consolidation, eta-expansion, inlining, let-expressions, multi-result
  primitives, the reduction of primitives, the separation of arguments,
  specialization, and the X-optimization. The strategy that
  applies them is "Clash.Normalize.Strategy.Spec".
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
