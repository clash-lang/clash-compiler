{-|
  Copyright  :  (C) 2012-2016, University of Twente
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Rewriting combinators and traversals for scoped Clash Core. Traversals open
  the binders they go under (see "Clash.Rewrite.Scoped.Types"), which renames
  a binder that clashes with the scope.

  See Note [topdown repeatR], Note [combinator inlining] and Note
  [topdownFixR] below.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Rewrite.Scoped.Combinators
  ( allR
  , descendLet
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

import           Control.Monad               ((>=>))
import qualified Control.Monad.Writer        as Writer
import qualified Data.Monoid                 as Monoid

import           Clash.Core.Scoped.Syntax
import           Clash.Rewrite.Scoped.Types

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

-- | Apply a transformation on the subtrees of a term. Opens the binders it
-- goes under.
allR :: forall m. Monad m => ScopedTransform m -> ScopedTransform m
allR (ScopedTransform trans) = ScopedTransform go
 where
  go :: Distinct n => TransformContext n -> Term n -> m (Term n)
  go ctx = \case
    Lam b e -> underLam ctx b e $ \ctx' b' e' -> Lam b' <$> trans ctx' e'
    TyLam b e -> underTyLam ctx b e $ \ctx' b' e' -> TyLam b' <$> trans ctx' e'
    App e1 e2 -> do
      e1' <- trans (pushFrame AppFun ctx) e1
      -- The argument's context is computed from the already rewritten
      -- function, like below
      e2' <- trans (pushFrame (appArgFrame e1') ctx) e2
      pure (App e1' e2')
    TyApp e ty -> (`TyApp` ty) <$> trans (pushFrame TyAppC ctx) e
    Cast e ty1 ty2 -> (\e' -> Cast e' ty1 ty2) <$> trans (pushFrame CastBody ctx) e
    Let bind e -> descendLet (ScopedTransform trans) ctx bind e $ \bind' e' -> pure (Let bind' e')
    Case scrut ty alts ->
      Case <$> trans (pushFrame CaseScrut ctx) scrut
           <*> pure ty
           <*> traverse rewriteAlt alts
     where
      rewriteAlt alt = underAlt ctx alt $ \ctx' p e -> Alt p <$> trans ctx' e
    Tick t e -> Tick t <$> trans (pushFrame (TickC t) ctx) e
    tm -> pure tm
{-# INLINABLE allR #-}

-- | Apply a transformation to the right-hand sides and the body of a
-- let-expression, given the let-expression's context, and pass the rebuilt
-- bindings and body on. The binding keeps its form: a 'NonRec' binder is in
-- scope of the body only, not of its own right-hand side.
descendLet
  :: (Monad m, Distinct n)
  => ScopedTransform m
  -- ^ The transformation to apply to the right-hand sides and the body
  -> TransformContext n
  -- ^ The context of the let-expression
  -> Bind n l
  -> Term l
  -> (forall l'. DExt n l' => Bind n l' -> Term l' -> m r)
  -> m r
descendLet (ScopedTransform trans) ctx bind e k = case bind of
  NonRec b x -> do
    x' <- trans (pushFrame (LetBinding b) ctx) x
    underLet ctx bind e $ \ctx' bind' e' -> case bind' of
      NonRec b' _ -> do
        e'' <- trans ctx' e'
        k (NonRec b' x') e''
      Rec {} -> error "descendLet: opening a NonRec gave a Rec"
  Rec bs xs ->
    underLetRec ctx bs xs e $ \ctxBody ctxRhs bs' xs' e' -> do
      xs'' <- sequence (zipWith (\i x -> trans (ctxRhs i) x) [0..] xs')
      e'' <- trans ctxBody e'
      k (Rec bs' xs'') e''
{-# INLINE descendLet #-}

infixr 6 >->
-- | Apply two transformations in succession
(>->) :: Monad m => ScopedTransform m -> ScopedTransform m -> ScopedTransform m
(>->) = \(ScopedTransform r1) (ScopedTransform r2) ->
  ScopedTransform (\c -> r1 c >=> r2 c)
{-# INLINE (>->) #-}

infixr 6 >-!->
-- | Apply two transformations in succession, and fully evaluate the term in
-- between
(>-!->) :: Monad m => ScopedTransform m -> ScopedTransform m -> ScopedTransform m
(>-!->) = \(ScopedTransform r1) (ScopedTransform r2) -> ScopedTransform $ \c e -> do
  e' <- r1 c e
  forceTerm e' `seq` r2 c e'
{-# INLINE (>-!->) #-}

-- | Evaluate a term fully, like 'Control.DeepSeq.deepseq' would. The fields of scoped terms are strict, except
-- for the lists of case alternatives and recursive right-hand sides, so a term
-- in WHNF is evaluated except for those.
forceTerm :: Term n -> ()
forceTerm = \case
  Lam _ e -> forceTerm e
  TyLam _ e -> forceTerm e
  App e1 e2 -> forceTerm e1 `seq` forceTerm e2
  TyApp e _ -> forceTerm e
  Let (NonRec _ x) e -> forceTerm x `seq` forceTerm e
  Let (Rec _ xs) e -> forceList forceTerm xs `seq` forceTerm e
  Case e _ alts -> forceTerm e `seq` forceList (\(Alt _ a) -> forceTerm a) alts
  Cast e _ _ -> forceTerm e
  Tick (Attributes _ a) e -> forceTerm a `seq` forceTerm e
  Tick _ e -> forceTerm e
  Var {} -> ()
  GlobalVar {} -> ()
  Data {} -> ()
  Literal {} -> ()
  Prim {} -> ()
 where
  forceList :: (a -> ()) -> [a] -> ()
  forceList f = foldr (\a r -> f a `seq` r) ()

-- | Apply a transformation in a topdown traversal
topdownR :: ScopedRewrite extra -> ScopedRewrite extra
-- See Note [topdown repeatR] and Note [combinator inlining]
topdownR r = go
 where
  go = repeatR r >-> allR go
{-# INLINE topdownR #-}

-- | Apply a transformation in a repeated top-down traversal. Optimized for
-- local, context-stable transformations, see Note [topdownFixR].
topdownFixR :: ScopedRewrite extra -> ScopedRewrite extra
topdownFixR r = go True
 where
  go tryParent = ScopedTransform $ \ctx term -> do
    term1 <-
      if tryParent
        then runScopedTransform (repeatR r) ctx term
        else pure term
    (term2, Monoid.getAny -> childChanged) <-
      Writer.listen (runScopedTransform (allR (go True)) ctx term1)
    if childChanged
      then do
        (term3, Monoid.getAny -> parentChanged) <-
          Writer.listen (runScopedTransform (repeatR r) ctx term2)
        if parentChanged
          then runScopedTransform (go False) ctx term3
          else return term3
      else return term2
{-# INLINE topdownFixR #-}

-- | Apply a transformation in a bottomup traversal
bottomupR :: Monad m => ScopedTransform m -> ScopedTransform m
-- See Note [combinator inlining]
bottomupR r = go
 where
  go = allR go >-> r
{-# INLINE bottomupR #-}

infixr 5 !->
-- | Only apply the second transformation if the first one succeeds
(!->) :: ScopedRewrite extra -> ScopedRewrite extra -> ScopedRewrite extra
(!->) = \(ScopedTransform r1) (ScopedTransform r2) -> ScopedTransform $ \c expr -> do
  (expr', changed) <- Writer.listen $ r1 c expr
  if Monoid.getAny changed
    then r2 c expr'
    else return expr'
{-# INLINE (!->) #-}

infixr 5 >-!
-- | Only apply the second transformation if the first one fails
(>-!) :: ScopedRewrite extra -> ScopedRewrite extra -> ScopedRewrite extra
(>-!) = \(ScopedTransform r1) (ScopedTransform r2) -> ScopedTransform $ \c expr -> do
  (expr', changed) <- Writer.listen $ r1 c expr
  if Monoid.getAny changed
    then return expr'
    else r2 c expr'
{-# INLINE (>-!) #-}

-- | Keep applying a transformation until it fails
repeatR :: ScopedRewrite extra -> ScopedRewrite extra
-- See Note [combinator inlining]
repeatR r = go
 where
  go = r !-> go
{-# INLINE repeatR #-}

-- | Topdown traversal, stops upon first success
topdownSucR :: ScopedRewrite extra -> ScopedRewrite extra
-- See Note [combinator inlining]
topdownSucR r = go
 where
  go = r >-! allR go
{-# INLINE topdownSucR #-}

-- | Bottomup traversal; when the transformation succeeds, re-traverse the
-- result until the innermost fixpoint is reached
innerMost :: ScopedRewrite extra -> ScopedRewrite extra
-- See Note [combinator inlining]
innerMost r = go
 where
  go = bottomupR (r !-> go)
{-# INLINE innerMost #-}
