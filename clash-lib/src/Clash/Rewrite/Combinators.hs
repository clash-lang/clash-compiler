{-|
  Copyright  :  (C) 2012-2016, University of Twente
                         2021, QBayLogic B.V.
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
  , repeatR
  , topdownR
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

-- | Apply a transformation in a topdown traversal
topdownR :: Rewrite m -> Rewrite m
-- See Note [topdown repeatR]
topdownR r = repeatR r >-> allR (topdownR r)

-- | Apply a transformation in a bottomup traversal
bottomupR :: Monad m => Transform m -> Transform m
bottomupR r = allR (bottomupR r) >-> r

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
repeatR = let go r = r !-> repeatR r in go
{-# INLINE repeatR #-}
