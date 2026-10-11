{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations on casts of scoped Clash Core: the ported counterparts of
  "Clash.Normalize.Transformations.Cast".
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Scoped.Transformations.Cast
  ( argCastSpec
  , argCastSpecWorker
  ) where

import qualified Control.Lens as Lens
import GHC.Stack (HasCallStack)

import Clash.Core.Pretty (showPpr)
import Clash.Driver.Warning (warnAboutM)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.StrategyDSL (Transformation, onApp, toScopedTransformation)
import Clash.Rewrite.Types (curFun)
import Clash.Util (curLoc)
import Clash.Warning (ClashWarning (WarnCastSpecialization))

import Clash.Core.Scoped.Pretty ()
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (collectArgs, stripTicks)
import Clash.Core.Scoped.TermInfo (isCast)
import Clash.Normalize.Scoped.Transformations.Specialize (specialize)
import Clash.Rewrite.Scoped.Globals (isWorkFreeTerm)
import Clash.Rewrite.Scoped.Types (TransformContext)

-- | Push a cast over an argument to a function into that function, by
-- specializing the function on the cast argument. See
-- 'Clash.Normalize.Transformations.Cast.argCastSpec'.
argCastSpec :: Transformation
argCastSpec = toScopedTransformation "argCastSpec" (onApp 'argCastSpecWorker)

-- | The 'App' handler of 'argCastSpec'
argCastSpecWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Term n
  -> NormalizeSession (Term n)
argCastSpecWorker ctx node f arg
  | Cast e' _ _ <- stripTicks arg
  -- Don't specialise when the arguments are casts-of-casts, these
  -- casts-of-casts will be eliminated by 'elimCastCast' during the
  -- normalization of the "current" function. We thus prevent the unnecessary
  -- introduction of a specialized version of 'f'.
  , not (isCast e')
  -- We can only push casts into global binders
  , (GlobalVar _, _) <- collectArgs f
  = isWorkFreeTerm e' >>= \case
      True -> specialize ctx node
      False -> warn >> specialize ctx node
  | otherwise = return node
 where
  warn = do
    (_, sp) <- Lens.use curFun
    warnAboutM WarnCastSpecialization sp $ unwords
      [ $(curLoc) ++ "specializing a function on a non work-free"
      , "cast. Generated HDL implementation might contain duplicate work."
      , "Please report this as a bug.", "\n\nExpression where this occured:"
      , "\n\n" ++ showPpr node
      ]
{-# SCC argCastSpecWorker #-}
