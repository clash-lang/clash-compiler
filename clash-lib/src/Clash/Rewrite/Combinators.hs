{-|
  Copyright  :  (C) 2012-2016, University of Twente
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Rewriting combinators and traversals for Clash Core. Traversals open
  the binders they go under (see "Clash.Rewrite.Types"), which renames
  a binder that clashes with the scope.

  See Note [topdown repeatR], Note [combinator inlining], Note
  [topdownFixR] and Note [sharing unchanged nodes] below.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Rewrite.Combinators
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
    -- * Sharing unchanged nodes
  , rebuildLam
  , rebuildTyLam
  , rebuildApp
  , rebuildTyApp
  , rebuildLet
  , rebuildCase
  , rebuildAlt
  , rebuildCast
  , rebuildTick
  ) where

import           Control.Monad               ((>=>))
import qualified Control.Monad.Writer        as Writer
import qualified Data.Monoid                 as Monoid
import           GHC.Exts                    (isTrue#, reallyUnsafePtrEquality#)

import           Clash.Core.Syntax
import           Clash.Rewrite.Types

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
-- goes under. Gives the term itself when no subtree changed, see Note [sharing
-- unchanged nodes].
allR :: forall m. Monad m => Transform m -> Transform m
allR (Transform trans) = Transform go
 where
  go :: Distinct n => TransformContext n -> Term n -> m (Term n)
  go ctx tm = case tm of
    Lam b e -> underLam ctx b e $ \ctx' b' e' -> do
      e'' <- trans ctx' e'
      pure $! rebuildLam tm b e b' e''
    TyLam b e -> underTyLam ctx b e $ \ctx' b' e' -> do
      e'' <- trans ctx' e'
      pure $! rebuildTyLam tm b e b' e''
    App e1 e2 -> do
      e1' <- trans (pushFrame AppFun ctx) e1
      -- The argument's context is computed from the already rewritten
      -- function, like below
      e2' <- trans (pushFrame (appArgFrame e1') ctx) e2
      pure $! rebuildApp tm e1 e2 e1' e2'
    TyApp e ty -> do
      e' <- trans (pushFrame TyAppC ctx) e
      pure $! rebuildTyApp tm e e' ty
    Cast e ty1 ty2 -> do
      e' <- trans (pushFrame CastBody ctx) e
      pure $! rebuildCast tm e e' ty1 ty2
    Let bind e -> descendLet (Transform trans) ctx bind e $ \bind' e' ->
      pure $! rebuildLet tm bind e bind' e'
    Case scrut ty alts -> do
      scrut' <- trans (pushFrame CaseScrut ctx) scrut
      alts' <- traverse rewriteAlt alts
      pure $! rebuildCase tm scrut alts scrut' ty alts'
     where
      rewriteAlt alt = underAlt ctx alt $ \ctx' p e -> do
        e' <- trans ctx' e
        pure $! rebuildAlt alt p e'
    Tick t e -> do
      e' <- trans (pushFrame (TickC t) ctx) e
      pure $! rebuildTick tm e t e'
    _ -> pure tm
{-# INLINABLE allR #-}

-- | Apply a transformation to the right-hand sides and the body of a
-- let-expression, given the let-expression's context, and pass the rebuilt
-- bindings and body on. The binding keeps its form: a 'NonRec' binder is in
-- scope of the body only, not of its own right-hand side.
descendLet
  :: (Monad m, Distinct n)
  => Transform m
  -- ^ The transformation to apply to the right-hand sides and the body
  -> TransformContext n
  -- ^ The context of the let-expression
  -> Bind n l
  -> Term l
  -> (forall l'. DExt n l' => Bind n l' -> Term l' -> m r)
  -> m r
descendLet (Transform trans) ctx bind e k = case bind of
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


{-
Note [sharing unchanged nodes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A traversal gives a node itself, rather than a copy, when the transformation
returned every child of the node unchanged, by pointer equality ('samePtr').
Most traversals change few nodes, so this saves rebuilding all the others,
and keeps the unchanged parts of a term shared with the term before the
traversal.

Going under a binder may rename it (see "Clash.Core.Subst"), which
gives a new binder and a new body. A copy then has the new names, so the
traversal rebuilds the node, as it did before: sharing never changes the term
a traversal gives, only whether it is a copy. Without renaming, opening a
binder gives the binder and the body themselves.

Pointer equality can say that two equal values are different, never the
reverse. A transformation that returns an equal copy just causes a rebuild.

The functions rebuilding a node ('rebuildApp' and so on) are NOINLINE, so
that their pointer comparisons happen where they are called, after the
children were transformed. Inlined, GHC's full laziness floats a comparison
that only needs values from before a monadic bind (the binders of a lambda,
the function of an application) out of the monadic lambdas, as a thunk.
-}

-- | Whether two values are the same object on the heap. 'False' doesn't mean
-- they differ. See Note [sharing unchanged nodes].
samePtr :: a -> b -> Bool
samePtr x y = isTrue# (reallyUnsafePtrEquality# x y)
{-# INLINE samePtr #-}

-- | Whether two lists have the same elements, by 'samePtr'
sameElems :: [a] -> [b] -> Bool
sameElems (x:xs) (y:ys) = samePtr x y && sameElems xs ys
sameElems [] [] = True
sameElems _ _ = False

-- | A lambda with the given binder and body, given the original node and its
-- binder and body: the original node when they are unchanged. See Note
-- [sharing unchanged nodes].
rebuildLam :: Term n -> IdBinder n l0 -> Term l0 -> IdBinder n l -> Term l -> Term n
rebuildLam tm b0 e0 b e
  | samePtr b0 b && samePtr e0 e = tm
  | otherwise = Lam b e
{-# NOINLINE rebuildLam #-}

-- | A type lambda, like 'rebuildLam'
rebuildTyLam :: Term n -> TyBinder n l0 -> Term l0 -> TyBinder n l -> Term l -> Term n
rebuildTyLam tm b0 e0 b e
  | samePtr b0 b && samePtr e0 e = tm
  | otherwise = TyLam b e
{-# NOINLINE rebuildTyLam #-}

-- | An application, like 'rebuildLam'
rebuildApp :: Term n -> Term n -> Term n -> Term n -> Term n -> Term n
rebuildApp tm f0 a0 f a
  | samePtr f0 f && samePtr a0 a = tm
  | otherwise = App f a
{-# NOINLINE rebuildApp #-}

-- | A type application, like 'rebuildLam'
rebuildTyApp :: Term n -> Term n -> Term n -> Type n -> Term n
rebuildTyApp tm f0 f ty
  | samePtr f0 f = tm
  | otherwise = TyApp f ty
{-# NOINLINE rebuildTyApp #-}

-- | A let-expression, like 'rebuildLam'. Its bindings are unchanged when
-- they have the same binders (so they weren't renamed) and the same
-- right-hand sides.
rebuildLet :: Term n -> Bind n l0 -> Term l0 -> Bind n l -> Term l -> Term n
rebuildLet tm bind0 e0 bind e
  | sameBind bind0 bind && samePtr e0 e = tm
  | otherwise = Let bind e
 where
  sameBind :: Bind n l0 -> Bind n l -> Bool
  sameBind (NonRec b0 x0) (NonRec b x) = samePtr b0 b && samePtr x0 x
  sameBind (Rec bs0 xs0) (Rec bs xs) = samePtr bs0 bs && sameElems xs0 xs
  sameBind _ _ = False
{-# NOINLINE rebuildLet #-}

-- | A case-expression, like 'rebuildLam', with alternatives rebuilt by
-- 'rebuildAlt'
rebuildCase :: Term n -> Term n -> [Alt n] -> Term n -> Type n -> [Alt n] -> Term n
rebuildCase tm s0 alts0 s ty alts
  | samePtr s0 s && sameElems alts0 alts = tm
  | otherwise = Case s ty alts
{-# NOINLINE rebuildCase #-}

-- | A case alternative, like 'rebuildLam'
rebuildAlt :: Alt n -> Pat n l -> Term l -> Alt n
rebuildAlt alt@(Alt p0 e0) p e
  | samePtr p0 p && samePtr e0 e = alt
  | otherwise = Alt p e
{-# NOINLINE rebuildAlt #-}

-- | A cast, like 'rebuildLam'
rebuildCast :: Term n -> Term n -> Term n -> Type n -> Type n -> Term n
rebuildCast tm e0 e ty1 ty2
  | samePtr e0 e = tm
  | otherwise = Cast e ty1 ty2
{-# NOINLINE rebuildCast #-}

-- | A tick, like 'rebuildLam'
rebuildTick :: Term n -> Term n -> TickInfo n -> Term n -> Term n
rebuildTick tm e0 t e
  | samePtr e0 e = tm
  | otherwise = Tick t e
{-# NOINLINE rebuildTick #-}

infixr 6 >->
-- | Apply two transformations in succession
(>->) :: Monad m => Transform m -> Transform m -> Transform m
(>->) = \(Transform r1) (Transform r2) ->
  Transform (\c -> r1 c >=> r2 c)
{-# INLINE (>->) #-}

infixr 6 >-!->
-- | Apply two transformations in succession, and fully evaluate the term in
-- between
(>-!->) :: Monad m => Transform m -> Transform m -> Transform m
(>-!->) = \(Transform r1) (Transform r2) -> Transform $ \c e -> do
  e' <- r1 c e
  forceTerm e' `seq` r2 c e'
{-# INLINE (>-!->) #-}

-- | Evaluate a term fully, like 'Control.DeepSeq.deepseq' would. The fields of terms are strict, except
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
topdownR :: Rewrite extra -> Rewrite extra
-- See Note [topdown repeatR] and Note [combinator inlining]
topdownR r = go
 where
  go = repeatR r >-> allR go
{-# INLINE topdownR #-}

-- | Apply a transformation in a repeated top-down traversal. Optimized for
-- local, context-stable transformations, see Note [topdownFixR].
topdownFixR :: Rewrite extra -> Rewrite extra
topdownFixR r = go True
 where
  go tryParent = Transform $ \ctx term -> do
    term1 <-
      if tryParent
        then runTransform (repeatR r) ctx term
        else pure term
    (term2, Monoid.getAny -> childChanged) <-
      Writer.listen (runTransform (allR (go True)) ctx term1)
    if childChanged
      then do
        (term3, Monoid.getAny -> parentChanged) <-
          Writer.listen (runTransform (repeatR r) ctx term2)
        if parentChanged
          then runTransform (go False) ctx term3
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
-- | Only apply the second transformation if the first one succeeds
(!->) :: Rewrite extra -> Rewrite extra -> Rewrite extra
(!->) = \(Transform r1) (Transform r2) -> Transform $ \c expr -> do
  (expr', changed) <- Writer.listen $ r1 c expr
  if Monoid.getAny changed
    then r2 c expr'
    else return expr'
{-# INLINE (!->) #-}

infixr 5 >-!
-- | Only apply the second transformation if the first one fails
(>-!) :: Rewrite extra -> Rewrite extra -> Rewrite extra
(>-!) = \(Transform r1) (Transform r2) -> Transform $ \c expr -> do
  (expr', changed) <- Writer.listen $ r1 c expr
  if Monoid.getAny changed
    then return expr'
    else r2 c expr'
{-# INLINE (>-!) #-}

-- | Keep applying a transformation until it fails
repeatR :: Rewrite extra -> Rewrite extra
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
-- result until the innermost fixpoint is reached
innerMost :: Rewrite extra -> Rewrite extra
-- See Note [combinator inlining]
innerMost r = go
 where
  go = bottomupR (r !-> go)
{-# INLINE innerMost #-}
