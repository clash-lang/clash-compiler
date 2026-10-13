{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Internal.Real" in the scoped evaluator.
  See "Clash.Core.Evaluator.Primitives" for how to write them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Core.Evaluator.Primitives.GHC.Internal.Real
  ( primitives
  ) where

import Data.Text (Text)

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types

primitives :: [(Text, PrimReduction)]
primitives =
  -- Which specialization each worker name implements shifts between GHC
  -- versions, so they all share one shape-dispatching implementation. See
  -- 'powImplWorker'.
  [ primStepEntry "GHC.Internal.Real.^_$s$spowImpl" powImplWorker
  , primStepEntry "GHC.Internal.Real.^_$s$spowImpl1" powImplWorker
  , primStepEntry "GHC.Internal.Real.^_$s$spowImpl2" powImplWorker
  , primStepEntry "GHC.Internal.Real.$w$spowImpl" powImplWorker
  , primStepEntry "GHC.Internal.Real.$w$spowImpl1" powImplWorker
  ]
