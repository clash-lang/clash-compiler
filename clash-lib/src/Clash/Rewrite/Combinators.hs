{-|
  Copyright  :  (C) 2012-2016, University of Twente
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Rewriting combinators and traversals
-}

module Clash.Rewrite.Combinators
  ( allR
  , (!->)
  , (>-!)
  , (>-!->)
  , (>->)
  , bottomupR
  , innerMost
  , repeatR
  , topdownR
  , topdownFixR
  , topdownSucR
  ) where

import           Control.DeepSeq             (deepseq)
import           Control.Monad               ((>=>))
import qualified Control.Monad.Writer        as Writer
import qualified Data.Monoid                 as Monoid

import           Clash.Core.Term             (Term (..), CoreContext (..), primArg, patIds)
import           Clash.Core.VarEnv
  (extendInScopeSet, extendInScopeSetList)
import           Clash.Rewrite.Types

-- | Apply a transformation on the subtrees of an term
allR
  :: forall m
   . Monad m
  => Transform m
  -- ^ The transformation to apply to the subtrees
  -> Transform m
allR trans (TransformContext is c) (Lam v e) =
  Lam v <$> trans (TransformContext (extendInScopeSet is v) (LamBody v:c)) e

allR trans (TransformContext is c) (TyLam tv e) =
  TyLam tv <$> trans (TransformContext (extendInScopeSet is tv) (TyLamBody tv:c)) e

allR trans (TransformContext is c) (App e1 e2) = do
  e1' <- trans (TransformContext is (AppFun:c)) e1
  e2' <- trans (TransformContext is (AppArg (primArg e1') : c)) e2
  pure (App e1' e2')

allR trans (TransformContext is c) (TyApp e ty) =
  TyApp <$> trans (TransformContext is (TyAppC:c)) e <*> pure ty

allR trans (TransformContext is c) (Cast e ty1 ty2) =
  Cast <$> trans (TransformContext is (CastBody:c)) e <*> pure ty1 <*> pure ty2

allR trans (TransformContext is c) (Letrec xes e) = do
  xes' <- traverse rewriteBind xes
  e'   <- trans (TransformContext is' (LetBody xes:c)) e
  return (Letrec xes' e')
 where
  bndrs              = map fst xes
  is'                = extendInScopeSetList is (map fst xes)
  rewriteBind (b,e') = (b,) <$> trans (TransformContext is' (LetBinding b bndrs:c)) e'

allR trans (TransformContext is c) (Case scrut ty alts) =
  Case <$> trans (TransformContext is (CaseScrut:c)) scrut
       <*> pure ty
       <*> traverse rewriteAlt alts
 where
  rewriteAlt (p,e) =
    let (tvs,ids) = patIds p
        is'       = extendInScopeSetList (extendInScopeSetList is tvs) ids
    in  (p,) <$> trans (TransformContext is' (CaseAlt p : c)) e

allR trans (TransformContext is c) (Tick sp e) =
  Tick sp <$> trans (TransformContext is (TickC sp:c)) e

allR _ _ tm = pure tm
{-# INLINABLE allR #-}

infixr 6 >->
-- | Apply two transformations in succession
(>->) :: Monad m => Transform m -> Transform m -> Transform m
(>->) = \r1 r2 c -> r1 c >=> r2 c
{-# INLINE (>->) #-}

infixr 6 >-!->
-- | Apply two transformations in succession, and perform a deepseq in between.
(>-!->) :: Monad m => Transform m -> Transform m -> Transform m
(>-!->) = \r1 r2 c e -> do
  e' <- r1 c e
  deepseq e' (r2 c e')
{-# INLINE (>-!->) #-}

{-
Note [topdown repeatR]
~~~~~~~~~~~~~~~~~~~~~~
In a topdown traversal we need to repeat the transformation r because
if r replaces a parent node with one of its children
we should still apply r to that child, before continuing with its children.

Example: topdownR (inlineBinders (\_ _ -> return True))
on:
> letrec
>   x = 1
> in letrec
>      y = 2
>    in f x y

inlineBinders would inline x and return:
> letrec
>   y = 2
> in f 1 y

Then we must repeat the transformation to let it also inline y.
-}

{-
Note [combinator inlining]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The traversal combinators carry INLINE pragmas so that a call site applying
them to a statically-known transformation gets its own copy of the traversal
loop: GHC then fuses the RWST/Writer plumbing into an unboxed state loop and
calls the transformation directly instead of through a function argument.

That only works when two conditions hold:

* The combinator is not (mutually) recursive at the top level. GHC picks a
  loop-breaker in every top-level recursive group and never inlines it, so an
  INLINE pragma on such a function is silently ignored. The recursion has to
  go through a local binding instead.

* The local loop closes over the transformation rather than passing it along
  as an argument. GHC does not specialize a loop on an invariant argument
  (the static argument transformation is off by default), so with
  'go r = r !-> go r' the call to 'r' stays a call to an unknown function;
  with 'go = r !-> go' the simplifier sees the concrete 'r' once the
  combinator is inlined.

'allR' is INLINABLE rather than INLINE: specializing it to the concrete monad
removes the per-node Monad-dictionary dispatch, while full inlining would
duplicate its per-constructor case alternatives at every call site for no
extra benefit.
-}

-- | Apply a transformation in a topdown traversal
topdownR :: Rewrite m -> Rewrite m
-- See Note [topdown repeatR] and Note [combinator inlining]
topdownR r = go
 where
  go = repeatR r >-> allR go
{-# INLINE topdownR #-}

{-
Note [topdownFixR]
~~~~~~~~~~~~~~~~~~
'topdownFixR r' is an optimized alternative to some uses of
'repeatR (topdownR r)'. It repeats 'r' top-down, but when a child changes it
only rechecks the ancestors of that child instead of restarting traversal from
the root.

For example, suppose 'r' can rewrite both:

> let x = True in x

to:

> True

and:

> case True of { True -> a; False -> b }

to:

> a

When traversing:

> h (case (let x = True in x) of { True -> a; False -> b })

'topdownFixR r' first cannot rewrite the 'case', so it descends into the
scrutinee. Rewriting the scrutinee exposes a new redex at the parent 'case', so
the parent is checked again immediately and rewritten to 'a'. That change then
bubbles up to 'h a'. With 'repeatR (topdownR r)' the same result is reached by
starting another complete traversal from 'h'.

Only use 'topdownFixR' as a replacement for 'repeatR (topdownR r)' when 'r' is
local and context-stable: it should fire or fail based on the current node, and
the relevant parts of 'TransformContext' should not change when sibling
subtrees are rewritten. Rewrites that inspect let-bound context whose binding
terms may have changed, for example through 'whnfRW', still need an outer
repeat or a normal repeated top-down traversal.

Note [topdownFixR is not for inlining bundles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'topdownFixR' trades traversals for re-descents, and that trade is only a win
when rewrites are cheap and rarely fire at the same node twice.

Whenever 'r' succeeds at a node, 'topdownFixR' re-descends into that node's
_entire_ subtree, because the rewrite may have restructured it. So one
'topdownFixR' costs

> n + sum over nodes v of (times r fired at v) * size of subtree(v)

node visits, where 'repeatR (topdownR r)' costs 'n' per round. 'topdownFixR'
wins when it saves enough rounds to pay for those re-descents.

For bundles that inline (@inlineWorkFree@, @inlineSmall@, @inlineOrLiftNonRep@)
it loses: inlining replaces a small node by an arbitrarily large body, every
enclosing node tends to become rewritable in turn, and each of those rewrites
re-descends a subtree that just grew. Measured on
@tests/shouldwork/Basic/T1354B.hs@ (a deeply nested chain of function
compositions), using 'topdownFixR' for 'inlineAndPropagate' reaches exactly the
same fixed point but needs 1.57x the node visits, costing ~40% wall-clock. It
was ~3% slower on a larger industrial design we've measured too. See #3250.
-}

-- | Apply a transformation in a repeated top-down traversal.
--
-- Optimized for local, context-stable transformations. See Note [topdownFixR].
topdownFixR :: Rewrite m -> Rewrite m
topdownFixR r = go True
 where
  go tryParent ctx term = do
    term1 <-
      if tryParent
        then repeatR r ctx term
        else pure term
    (term2, Monoid.getAny -> childChanged) <- Writer.listen (allR (go True) ctx term1)
    if childChanged
      then do
        (term3, Monoid.getAny -> parentChanged) <- Writer.listen (repeatR r ctx term2)
        if parentChanged
          then go False ctx term3
          else return term3
      else return term2
{-# INLINE topdownFixR #-}

-- | Apply a transformation in a bottomup traversal
bottomupR :: Monad m => Transform m -> Transform m
-- See Note [combinator inlining]
bottomupR r = go
 where
  go = allR go >-> r
{-# INLINE bottomupR #-}

infixr 5 !->
-- | Only apply the second transformation if the first one succeeds.
(!->) :: Rewrite m -> Rewrite m -> Rewrite m
(!->) = \r1 r2 c expr -> do
  (expr',changed) <- Writer.listen $ r1 c expr
  if Monoid.getAny changed
    then r2 c expr'
    else return expr'
{-# INLINE (!->) #-}

infixr 5 >-!
-- | Only apply the second transformation if the first one fails.
(>-!) :: Rewrite m -> Rewrite m -> Rewrite m
(>-!) = \r1 r2 c expr -> do
  (expr',changed) <- Writer.listen $ r1 c expr
  if Monoid.getAny changed
    then return expr'
    else r2 c expr'
{-# INLINE (>-!) #-}

-- | Keep applying a transformation until it fails.
repeatR :: Rewrite m -> Rewrite m
-- See Note [combinator inlining]
repeatR r = go
 where
  go = r !-> go
{-# INLINE repeatR #-}

-- | Topdown traversal, stops upon first success
topdownSucR :: Rewrite extra -> Rewrite extra
-- See Note [combinator inlining]
topdownSucR r = go
 where
  go = r >-! allR go
{-# INLINE topdownSucR #-}

-- | Bottomup traversal; when the transformation succeeds, re-traverse the
-- result until the innermost fixpoint is reached.
innerMost :: Rewrite extra -> Rewrite extra
-- See Note [combinator inlining]
innerMost r = go
 where
  go = bottomupR (r !-> go)
{-# INLINE innerMost #-}
