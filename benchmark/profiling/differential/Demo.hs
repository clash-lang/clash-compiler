{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Functions that demonstrate the harness, until there are real ports: a port
  of 'Clash.Normalize.Transformations.Case.caseOneAlt', a deliberately broken
  variant of it, and a broken whole-term function.

  The port of @caseOneAlt@ is only a demonstration: the real one belongs in
  @Clash.Normalize.Scoped.Transformations.Case@.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Demo
  ( caseOneAltT
  , caseOneAltWorker
  , caseOneAltS
  , caseOneAltBrokenS
  , dropTicks
  , ticksKnownDifference
  ) where

import Data.Maybe (mapMaybe)

import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Normalize.Transformations.Case (caseOneAlt)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.StrategyDSL (Transformation, onCase, toTransformation)
import Clash.Rewrite.Types (TransformContext)
import Clash.Rewrite.Util (changed)

import Clash.Core.Scoped.Alpha (commonAltBody)
import Clash.Core.Scoped.Subst (unsinkAlt)
import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped (ScopedRewrite, ScopedTransform (..))

import Test.Clash.Differential (KnownDifference (..), everywhereTm)

-- | 'caseOneAlt' as a transformation of its own. The normalizer only runs it
-- as part of 'Clash.Normalize.Transformations.Case.caseCon'.
caseOneAltT :: Transformation
caseOneAltT = toTransformation "caseOneAlt" (onCase 'caseOneAltWorker)

-- | The 'C.Case' handler of 'caseOneAltT'
caseOneAltWorker
  :: TransformContext -> C.Term -> C.Term -> C.Type -> [C.Alt] -> NormalizeSession C.Term
caseOneAltWorker _ node _ _ _ = caseOneAlt node

-- | Port of 'caseOneAlt': a case-expression with one alternative, or whose
-- alternatives are all the same, is that alternative, if it doesn't mention
-- the variables its pattern binds
caseOneAltS :: ScopedRewrite extra
caseOneAltS = ScopedTransform $ \_ -> \case
  e@(Case _ _ [Alt p a]) -> maybe (pure e) changed (unsinkAlt p a)
  e@(Case _ _ alts@(_:_:_)) -> maybe (pure e) changed (commonAltBody alts)
  e -> pure e

-- | 'caseOneAltS', but broken on purpose: of a case-expression with several
-- alternatives, it keeps the first one that doesn't mention the variables its
-- pattern binds, without checking that the others are the same
caseOneAltBrokenS :: ScopedRewrite extra
caseOneAltBrokenS = ScopedTransform $ \ctx -> \case
  e@(Case _ _ alts@(_:_:_)) ->
    case mapMaybe (\(Alt p a) -> unsinkAlt p a) alts of
      a : _ -> changed a
      [] -> pure e
  e -> runScopedTransform caseOneAltS ctx e

-- | Remove all ticks
dropTicks :: Term n -> Term n
dropTicks = everywhereTm $ \case
  Tick _ e -> e
  e -> e

-- | Ticks don't matter
ticksKnownDifference :: KnownDifference
ticksKnownDifference = KnownDifference
  { kdName = "ticks"
  , kdDescription = "ticks are left out"
  , kdNormalize = \_ -> dropTicks
  }
