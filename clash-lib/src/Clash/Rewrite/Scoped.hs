{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Writing transformations of scoped Clash Core ("Clash.Core.Scoped.Syntax").
  This module re-exports what a transformation needs from the rewrite
  machinery.
-}

module Clash.Rewrite.Scoped
  ( module Clash.Rewrite.Scoped.Types
  , module Clash.Rewrite.Scoped.Combinators
  , module Clash.Rewrite.Scoped.Apply
  , changed
  , setChanged
  ) where

import Clash.Rewrite.Scoped.Apply
import Clash.Rewrite.Scoped.Combinators
import Clash.Rewrite.Scoped.Types
import Clash.Rewrite.Util (changed, setChanged)
