{-|
Copyright   : (C) 2020, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

The partial evalautor for the GHC front-end. This can be used to evaluate
terms in Clash core to WHNF or NF, using knowledge of GHC primitives and types.
For functions which can use this evaluator, see Clash.Core.PartialEval.
-}

module Clash.GHC.PartialEval where

import Clash.Core.PartialEval

import Clash.GHC.PartialEval.Eval
import Clash.GHC.PartialEval.Quote

-- | The partial evaluator for the GHC front-end. For more details about the
-- implementation see Clash.GHC.PartialEval.Eval for evaluation to WHNF and
-- Clash.GHC.PartialEval.Quote for quoting to NF.
--
ghcEvaluator :: Evaluator
ghcEvaluator = Evaluator eval quote

