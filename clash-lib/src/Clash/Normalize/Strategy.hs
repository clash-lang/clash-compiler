{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                (C) 2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformation process for normalization, on scoped Clash Core. The
  strategies themselves are described in "Clash.Normalize.Strategy.Spec".
-}

{-# LANGUAGE TemplateHaskell #-}

--{-# OPTIONS_GHC -ddump-splices #-}

module Clash.Normalize.Strategy
  ( normalization
  , constantPropagation
  ) where

import Clash.Normalize.Strategy.Spec
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Scoped.Types (Rewrite)
import Clash.Rewrite.StrategyDSL.TH (compileScopedStrategy)

constantPropagation :: Rewrite NormalizeState
constantPropagation = $(compileScopedStrategy constantPropagationSpec)

-- constantPropagation is defined first: the splice below quotes its name,
-- and a top-level splice can only see the declarations above it.

-- | Normalisation transformation
normalization :: Rewrite NormalizeState
normalization = $(compileScopedStrategy (normalizationSpec 'constantPropagation))
