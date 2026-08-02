{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                (C) 2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformation process for normalization. The strategies themselves are
  described in "Clash.Normalize.Strategy.Spec"; this module compiles them
  into fused dispatch code with "Clash.Rewrite.StrategyDSL.Compile".
-}

module Clash.Normalize.Strategy
  ( normalization
  , constantPropagation
  ) where

import Clash.Normalize.Strategy.Spec
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.Compile (compile)

constantPropagation :: NormRewrite
constantPropagation = compile constantPropagationSpec

-- | Normalisation transformation
normalization :: NormRewrite
normalization = compile (normalizationSpec constantPropagation)
