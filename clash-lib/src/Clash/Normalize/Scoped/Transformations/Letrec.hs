{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations on let-expressions of scoped Clash Core: the ported
  counterparts of "Clash.Normalize.Transformations.Letrec".
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Scoped.Transformations.Letrec
  ( deadCode
  , deadCodeWorker
  ) where

import GHC.Stack (HasCallStack)

import Clash.Core.Scoped.Syntax
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.Scoped (TransformContext, changed, tfScope)
import Clash.Rewrite.Scoped.Util (removeUnusedBinders)
import Clash.Rewrite.StrategyDSL
  (Transformation, onLet, toScopedTransformation)

-- | Remove unused let-bindings
deadCode :: Transformation
deadCode = toScopedTransformation "deadCode" (onLet 'deadCodeWorker)

-- | The 'Let' handler of 'deadCode'
deadCodeWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Bind n l
  -> Term l
  -> NormalizeSession (Term n)
deadCodeWorker ctx e bind body =
  case removeUnusedBinders (tfScope ctx) bind body of
    Just t -> changed t
    Nothing -> return e
{-# SCC deadCodeWorker #-}
