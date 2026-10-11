{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Evaluating a subterm of a scoped rewrite to weak-head normal form, with the
  evaluator of scoped Clash Core ("Clash.Core.Scoped.Evaluator"). The scoped
  counterpart of 'Clash.Rewrite.Util.whnfRW'.

  The evaluator gets the let-bindings of the context ('tfHeap') as its local
  heap. Its result lives in the scope of the subterm extended with the
  bindings it allocated, so the transformation continues in the context
  extended with those bindings, and its result is bound by them.

  Inlining the work-free ones among those bindings and removing the unused
  ones ('Clash.Rewrite.Util.cleanupHeapBindings') is not ported yet: it runs
  on Clash Core, converting the result of the transformation there and back.
  That only happens when the transformation changed something.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Rewrite.Scoped.Whnf
  ( whnfRW
  , evaluatorEnv
  ) where

import qualified Control.Lens as Lens
import Control.Monad.State.Strict (runState)
import qualified Control.Monad.Writer as Writer
import qualified Data.Monoid as Monoid
import GHC.Stack (HasCallStack)

import qualified Clash.Core.Evaluator.Types as C
import qualified Clash.Core.Term as C
import Clash.Core.VarEnv (emptyInScopeSet, extendInScopeSetList)
import qualified Clash.Rewrite.Types as C
import Clash.Rewrite.Types
  (RewriteMonad, bindings, evaluator, globalHeap, scopedTypeEnv)
import Clash.Rewrite.Util (cleanupHeapBindings)
import Clash.Util (curLoc)

import Clash.Core.Scoped.Evaluator (allocatedBind, whnf)
import Clash.Core.Scoped.Convert (fromClashTermWith)
import Clash.Core.Scoped.Evaluator.Adapter
  (adaptPrim, clashVarsIds, clashVarsTyVars, emptyClashVars, toClashTermWith)
import Clash.Core.Scoped.Evaluator.Primitives (scopedPrimReductions)
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.FreeVars (bindNameBinders)
import Clash.Core.Scoped.Subst (unsinkTerm)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TypeEnv (TypeEnv)
import Clash.Rewrite.Scoped.Types

-- | The environment of the scoped evaluator: the ported primitive reductions,
-- and the others with the evaluator of Clash Core, see
-- "Clash.Core.Scoped.Evaluator.Adapter"
evaluatorEnv :: TypeEnv -> C.Evaluator -> Env
evaluatorEnv te eval =
  Env te (PrimReductions scopedPrimReductions (Just (adaptPrim eval)))

-- | Evaluate a term to weak-head normal form, and apply a transformation to
-- the result, in the context extended with the bindings the evaluator
-- allocated. See 'Clash.Rewrite.Util.whnfRW', and the module header.
--
-- If the transformation signals a change, its result is bound by those
-- bindings, of which the work-free ones are inlined, and the unused ones
-- removed, like 'Clash.Rewrite.Util.bindPureHeap' does. Otherwise the result
-- is the one of the transformation, bound by those bindings only if it
-- mentions them: return (the 'sink' of) the original term when nothing
-- changes.
whnfRW
  :: (HasCallStack, Distinct n)
  => Bool
  -- ^ Whether the term is the subject of a case-expression
  -> TransformContext n
  -> Term n
  -> (forall l. DExt n l => TransformContext l -> Term l -> RewriteMonad extra (Term l))
  -> RewriteMonad extra (Term n)
whnfRW isSubj ctx e k = do
  te <- Lens.view scopedTypeEnv
  eval <- Lens.view evaluator
  bm <- Lens.use bindings
  ph <- Lens.use globalHeap
  let sc = tfScope ctx
      m0 = initMachine sc (tfHeap ctx) bm ph e
  whnf (evaluatorEnv te eval) isSubj m0 $ \m -> do
    globalHeap Lens..= mHeapPrim m
    case mAlloc m of
      ANil -> k ctx (mTerm m)
      ASnoc {} -> do
        let bind = allocatedBind m
            ctx1 = pushUnder (mScope m) (LetBody bind) ctx
        (r, Monoid.getAny -> changed) <- Writer.listen (k ctx1 (mTerm m))
        if changed
          then cleanupAllocated sc (Let bind r)
          else case unsinkTerm (bindNameBinders bind) r of
            Just r' -> pure r'
            Nothing -> pure (Let bind r)

-- | Inline the work-free ones among the bindings the evaluator allocated,
-- and remove the unused ones, see 'Clash.Rewrite.Util.cleanupHeapBindings'.
-- On Clash Core: see the module header.
cleanupAllocated
  :: (HasCallStack, Distinct n) => Scope n -> Term n -> RewriteMonad extra (Term n)
cleanupAllocated sc t = case tC of
  C.Let (C.Rec bs) r ->
    fromClashTermWith sc tvs ids <$> cleanupHeapBindings (C.TransformContext is []) bs r
  _ -> error ($(curLoc) ++ "expected a recursive let-binding")
 where
  (tC, vars) = runState (toClashTermWith t) emptyClashVars
  tvs = clashVarsTyVars vars
  ids = clashVarsIds vars
  is = extendInScopeSetList (extendInScopeSetList emptyInScopeSet (map fst tvs)) (map fst ids)
