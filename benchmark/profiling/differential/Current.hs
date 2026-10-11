{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The current implementations of transformations that the normalizer runs
  ported versions of: porting a transformation swaps the one that the
  strategies use (see "Clash.Rewrite.Scoped"), but leaves the worker of the
  current one in place until Phase 7 of the port.
-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Current
  ( deadCode
  ) where

import qualified Clash.Normalize.Transformations.Letrec as Letrec
import Clash.Rewrite.StrategyDSL (Transformation, onLet, toTransformation)

-- | The current 'Clash.Normalize.Transformations.Letrec.deadCode', which
-- 'Clash.Normalize.Scoped.Transformations.Letrec.deadCode' replaced
deadCode :: Transformation
deadCode = toTransformation "deadCode" (onLet 'Letrec.deadCodeWorker)
