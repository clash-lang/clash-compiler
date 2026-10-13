{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2017     , Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Instrumentation of scoped transformations: the counterparts of
  'Clash.Rewrite.Util.apply', 'Clash.Rewrite.Util.applyWith' and
  'Clash.Rewrite.Util.applyDebug'.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Rewrite.Scoped.Apply
  ( apply
  , applyWith
  , applyDebug
  , runRewrite
  ) where

import           Control.Lens                ((+=))
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import           Control.Monad.IO.Class      (liftIO)
import qualified Control.Monad.Writer        as Writer
import           Data.Binary                 (encode)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import           Data.Maybe                  (fromJust, fromMaybe, isJust)
import qualified Data.Monoid                 as Monoid

import           Clash.Core.HasType          (inferCoreTypeOf)
import           Clash.Core.Pretty           (showPpr)
import           Clash.Core.Subst            (aeqType)
import           Clash.Core.Type             (normalizeType)
import           Clash.Debug                 (traceIf, traceM)
import           Clash.Driver.Types
  (DebugOpts (..), TransformationInfo (..), hasDebugInfo, isDebugging)
import           Clash.Rewrite.Types
  (RewriteMonad, RewriteStep (..), curFun, debugOpts, tcCache,
   transformAppliedCounters, transformCounter, transformTriedCounters)
import           Clash.Rewrite.Util          (bumpCounter)
import           Clash.Util                  (curLoc)

import           Clash.Core.Scoped.Alpha     (aeqTm)
import           Clash.Core.Scoped.Convert   (toClashTerm)
import           Clash.Core.Scoped.Syntax
import           Clash.Rewrite.Scoped.Types

-- | Record if a transformation is successfully applied, see
-- 'Clash.Rewrite.Util.apply'
apply
  :: String
  -- ^ Name of the transformation
  -> ScopedRewrite extra
  -- ^ Transformation to be applied
  -> ScopedRewrite extra
apply = \s (ScopedTransform rewrite) -> ScopedTransform $ \ctx expr0 ->
  fst <$> applyWith s ctx expr0 (rewrite ctx expr0)
{-# INLINE apply #-}

-- | The instrumentation core of 'apply' (debug tracing, transformation
-- counter, rewrite history, invariant checks) around an already-applied
-- transformation action. Also returns whether the action signaled change.
-- See 'Clash.Rewrite.Util.applyWith'.
applyWith
  :: Distinct n
  => String
  -- ^ Name of the transformation
  -> TransformContext n
  -> Term n
  -- ^ The node the transformation was offered
  -> RewriteMonad extra (Term n)
  -- ^ The transformation, applied to that node
  -> RewriteMonad extra (Term n, Bool)
applyWith = \s _ctx expr0 action -> do
  opts <- Lens.view debugOpts
  traceIf (hasDebugInfo TryName s opts) ("Trying: " <> s) (pure ())

  (!expr1, anyChanged) <- Writer.listen action
  let hasChanged = Monoid.getAny anyChanged
  Monad.when hasChanged (transformCounter += 1)

  -- NB: When -fclash-debug-history is on, emit binary data holding the
  -- recorded rewrite steps. The terms are converted to Clash Core, with the
  -- raw names of their variables as uniques. The context is not recorded.
  let rewriteHistFile = dbg_historyFile opts
  Monad.when (isJust rewriteHistFile && hasChanged) $ do
    (curBndr, _) <- Lens.use curFun
    liftIO
      $ BS.appendFile (fromJust rewriteHistFile)
      $ BL.toStrict
      $ encode RewriteStep
          { t_ctx    = []
          , t_name   = s
          , t_bndrS  = showPpr (globalIdName curBndr)
          , t_before = toClashTerm expr0
          , t_after  = toClashTerm expr1
          }

  if isDebugging opts
    then (, hasChanged) <$> applyDebug s expr0 hasChanged expr1
    else return (expr1, hasChanged)
{-# INLINE applyWith #-}

-- | The debug checks and traces of a transformation, see
-- 'Clash.Rewrite.Util.applyDebug'. Of its invariant checks, these remain:
--
-- * The transformation changed the term without signaling it: the result is
--   not alpha-equivalent to the original.
--
-- * The transformation changed the type of the term (traced, like in
--   'Clash.Rewrite.Util.applyDebug').
--
-- These are left out because the type checker enforces them for scoped Core:
--
-- * Free variables that the transformation introduced: a result in scope @n@
--   only mentions variables of @n@.
--
-- * Let-bindings and patterns that bind a variable twice, and the scope lint
--   ("Clash.Core.Lint"): binders in a 'Distinct' scope never shadow one
--   another, and every variable occurrence is in scope.
applyDebug
  :: String
  -- ^ Name of the transformation
  -> Term n
  -- ^ Original expression
  -> Bool
  -- ^ Whether the rewrite indicated change
  -> Term n
  -- ^ New expression
  -> RewriteMonad extra (Term n)
applyDebug name exprOld hasChanged exprNew = do
  nTrans <- Lens.use transformCounter
  opts <- Lens.view debugOpts

  let from = fromMaybe 0 (dbg_transformationsFrom opts)
  let limit = fromMaybe maxBound (dbg_transformationsLimit opts)

  if | nTrans - from > limit ->
         error "-fclash-debug-transformations-limit exceeded"
     | nTrans <= from ->
         pure exprNew
     | otherwise ->
         go opts
 where
  go opts = traceIf (hasDebugInfo TryTerm name opts) ("Tried: " ++ name ++ " on:\n" ++ before) $ do
    nTrans <- pred <$> Lens.use transformCounter

    Monad.when (dbg_countTransformations opts) $ do
      bumpCounter name transformTriedCounters
      Monad.when hasChanged $
        bumpCounter name transformAppliedCounters

    -- The checks of free variables, of accidental shadowing, and the scope
    -- lint are enforced by the type checker, see the haddock of this
    -- function.
    Monad.when (dbg_invariants opts && hasChanged) $ do
      tcm <- Lens.view tcCache
      let beforeTy = inferCoreTypeOf tcm (toClashTerm exprOld)
          afterTy = inferCoreTypeOf tcm (toClashTerm exprNew)
      -- Like in 'Clash.Rewrite.Util.applyDebug', a trace rather than an error
      Monad.when (hasDebugInfo AppliedTerm name opts && not (normalizeType tcm beforeTy `aeqType` normalizeType tcm afterTy)) $
        traceM ( concat [ $(curLoc)
                        , "Error when applying rewrite ", name
                        , " to:\n" , before
                        , "\nResult:\n" ++ after ++ "\n"
                        , "Changes type from:\n", showPpr beforeTy
                        , "\nto:\n", showPpr afterTy
                        ]
               )

    Monad.when (dbg_invariants opts && not hasChanged && not (aeqTm exprOld exprNew)) $
      error $ $(curLoc) ++ "Expression changed without notice(" ++ name ++  "): before"
                        ++ before ++ "\nafter:\n" ++ after

    traceIf (hasDebugInfo AppliedName name opts && hasChanged) (name <> " {" <> show nTrans <> "}") $
      traceIf (hasDebugInfo AppliedTerm name opts && hasChanged) ("Changes when applying rewrite to:\n"
                        ++ before ++ "\nResult:\n" ++ after ++ "\n") $
        traceIf (hasDebugInfo TryTerm name opts && not hasChanged) ("No changes when applying rewrite "
                          ++ name ++ " to:\n" ++ after ++ "\n") $
          return exprNew
   where
    before = showPpr (toClashTerm exprOld)
    after  = showPpr (toClashTerm exprNew)

-- | Perform a transformation on a term in the root of its scope, see
-- 'Clash.Rewrite.Util.runRewrite'
runRewrite
  :: Distinct n
  => String
  -- ^ Name of the transformation
  -> Scope n
  -> ScopedRewrite extra
  -- ^ Transformation to perform
  -> Term n
  -- ^ Term to transform
  -> RewriteMonad extra (Term n)
runRewrite name sc rewrite = runScopedTransform (apply name rewrite) (rootContext sc)
