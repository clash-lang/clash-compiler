{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Evaluating a subterm of a scoped rewrite to weak-head normal form, with the
  evaluator of scoped Clash Core ("Clash.Core.Evaluator").

  The evaluator gets the let-bindings of the context ('tfHeap') as its local
  heap. Its result lives in the scope of the subterm extended with the
  bindings it allocated, so the transformation continues in the context
  extended with those bindings, and its result is bound by them.

  When the transformation changed something, the work-free ones among those
  bindings are inlined and the unused ones removed, see
  'Clash.Rewrite.Util.bindPureHeap'.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Rewrite.Whnf
  ( whnfRW
  , evaluatorEnv
  ) where

import qualified Control.Lens as Lens
import GHC.Stack (HasCallStack)


import Clash.Core.Evaluator (allocatedBind, whnf)
import Clash.Core.Evaluator.Primitives (primReductionMap)
import Clash.Core.Evaluator.Types
import Clash.Core.Syntax
import Clash.Core.TypeEnv (TypeEnv)
import Clash.Rewrite.Types
import Clash.Rewrite.Util (bindPureHeap)

-- | The environment of the scoped evaluator: the primitive reductions of
-- "Clash.Core.Evaluator.Primitives". A primitive without one doesn't
-- reduce.
evaluatorEnv :: TypeEnv -> Env
evaluatorEnv te = Env te (PrimReductions primReductionMap Nothing)

-- | Evaluate a term to weak-head normal form, and apply a transformation to
-- the result, in the context extended with the bindings the evaluator
-- allocated. See the module header.
--
-- If the transformation signals a change, its result is bound by those
-- bindings, of which the work-free ones are inlined, and the unused ones
-- removed ('Clash.Rewrite.Util.bindPureHeap'). Otherwise the result
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
  te <- Lens.view typeEnv
  bm <- Lens.use bindings
  ph <- Lens.use globalHeap
  let sc = tfScope ctx
      m0 = initMachine sc (tfHeap ctx) bm ph e
  whnf (evaluatorEnv te) isSubj m0 $ \m -> do
    globalHeap Lens..= mHeapPrim m
    case mAlloc m of
      ANil -> k ctx (mTerm m)
      ASnoc {} -> bindPureHeap ctx (allocatedBind m) (mTerm m) k
