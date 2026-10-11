{-|
  Copyright  :  (C) 2012-2016, University of Twente
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Rewriting combinators and traversals for scoped Clash Core: the
  counterparts of "Clash.Rewrite.Combinators". They behave the same, except
  that traversals open the binders they go under (see
  "Clash.Rewrite.Scoped.Types"), which renames a binder that clashes with the
  scope.

  See Note [topdown repeatR], Note [combinator inlining] and Note
  [topdownFixR] in "Clash.Rewrite.Combinators".
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
      -- function, like in "Clash.Rewrite.Combinators"
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

-- | Evaluate a term fully, like 'Control.DeepSeq.deepseq' does in
-- "Clash.Rewrite.Combinators". The fields of scoped terms are strict, except
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
-- See Note [topdown repeatR] and Note [combinator inlining] in
-- "Clash.Rewrite.Combinators"
topdownR r = go
 where
  go = repeatR r >-> allR go
{-# INLINE topdownR #-}

-- | Apply a transformation in a repeated top-down traversal. Optimized for
-- local, context-stable transformations, see Note [topdownFixR] in
-- "Clash.Rewrite.Combinators".
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
-- See Note [combinator inlining] in "Clash.Rewrite.Combinators"
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
-- See Note [combinator inlining] in "Clash.Rewrite.Combinators"
repeatR r = go
 where
  go = r !-> go
{-# INLINE repeatR #-}

-- | Topdown traversal, stops upon first success
topdownSucR :: ScopedRewrite extra -> ScopedRewrite extra
-- See Note [combinator inlining] in "Clash.Rewrite.Combinators"
topdownSucR r = go
 where
  go = r >-! allR go
{-# INLINE topdownSucR #-}

-- | Bottomup traversal; when the transformation succeeds, re-traverse the
-- result until the innermost fixpoint is reached
innerMost :: ScopedRewrite extra -> ScopedRewrite extra
-- See Note [combinator inlining] in "Clash.Rewrite.Combinators"
innerMost r = go
 where
  go = bottomupR (r !-> go)
{-# INLINE innerMost #-}
