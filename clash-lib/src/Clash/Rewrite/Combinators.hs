{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Rewriting combinators and traversals
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Clash.Rewrite.Combinators where

import           Control.DeepSeq             (deepseq)
import           Control.Monad               ((<=<), (>=>))
import qualified Control.Monad.Writer        as Writer
import qualified Data.Monoid                 as Monoid
import           Unbound.Generics.LocallyNameless (Embed, Fresh, bind, embed,
                                                   rec, unbind, unembed, unrec)
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Clash.Core.Term             (Pat, Term (..))
import           Clash.Core.Util             (patIds)
import           Clash.Core.Var              (Id)
import           Clash.Rewrite.Types

-- | Apply a transformation on the subtrees of an term
allR :: forall m . (Monad m, Fresh m)
     => Bool -- ^ Freshen variable references in abstracted terms
     -> Transform m -- ^ The transformation to apply to the subtrees
     -> Transform m
allR _ _ _ (Var t x)   = return (Var t x)
allR _ _ _ (Data dc)   = return (Data dc)
allR _ _ _ (Literal l) = return (Literal l)
allR _ _ _ (Prim nm t) = return (Prim nm t)

allR rf trans c (Lam b) = do
  (v,e) <- if rf then unbind b else return (unsafeUnbind b)
  e'    <- trans (LamBody v:c) e
  return . Lam $ bind v e'

allR rf trans c (TyLam b) = do
  (tv, e) <- if rf then unbind b else return (unsafeUnbind b)
  e' <- trans (TyLamBody tv:c) e
  return . TyLam $ bind tv e'

allR _ trans c (App e1 e2) = do
  e1' <- trans (AppFun:c) e1
  e2' <- trans (AppArg:c) e2
  return $ App e1' e2'

allR _ trans c (TyApp e ty) = do
  e' <- trans (TyAppC:c) e
  return $ TyApp e' ty

allR _ trans c (Cast e ty1 ty2) = do
  e' <- trans (CastBody:c) e
  return $ Cast e' ty1 ty2

allR rf trans c (Letrec b) = do
  (xesR,e) <- if rf then unbind b else return (unsafeUnbind b)
  let xes   = unrec xesR
  let bndrs = map fst xes
  e' <- trans (LetBody bndrs:c) e
  xes' <- mapM (rewriteBind bndrs) xes
  return . Letrec $ bind (rec xes') e'
  where
    rewriteBind :: [Id] -> (Id,Embed Term) -> m (Id,Embed Term)
    rewriteBind bndrs (b', e) = do
      e' <- trans (LetBinding b' bndrs:c) (unembed e)
      return (b',embed e')

allR rf trans c (Case scrut ty alts) = do
  scrut' <- trans (CaseScrut:c) scrut
  alts'  <- if rf then mapM (fmap (uncurry bind) . rewriteAlt <=< unbind) alts
                  else mapM (fmap (uncurry bind) . rewriteAlt . unsafeUnbind) alts
  return $ Case scrut' ty alts'
  where
    rewriteAlt :: (Pat, Term) -> m (Pat, Term)
    rewriteAlt (p,e) = do
      let (tvs,ids) = patIds p
      e' <- trans (CaseAlt tvs ids:c) e
      return (p,e')

infixr 6 >->
-- | Apply two transformations in succession
(>->) :: Monad m => Transform m -> Transform m -> Transform m
(>->) r1 r2 c = r1 c >=> r2 c

infixr 6 >-!->
-- | Apply two transformations in succession, and perform a deepseq in between.
(>-!->) :: Monad m => Transform m -> Transform m -> Transform m
(>-!->) r1 r2 c e = do
  e' <- r1 c e
  deepseq e' (r2 c e')

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
topdownR r = repeatR r >-> allR True (topdownR r)

-- | Apply a transformation in a topdown traversal. Doesn't freshen bound
-- variables
unsafeTopdownR :: Rewrite m -> Rewrite m
-- See NOTE [topdown repeatR]
unsafeTopdownR r = repeatR r >-> allR False (unsafeTopdownR r)

-- | Apply a transformation in a bottomup traversal
bottomupR :: Fresh m => Transform m -> Transform m
bottomupR r = allR True (bottomupR r) >-> r

-- | Apply a transformation in a bottomup traversal. Doesn't freshen bound
-- variables
unsafeBottomupR :: Fresh m => Transform m -> Transform m
unsafeBottomupR r = allR False (unsafeBottomupR r) >-> r

infixr 5 !->
-- | Only apply the second transformation if the first one succeeds.
(!->) :: Rewrite m -> Rewrite m -> Rewrite m
(!->) r1 r2 c expr = do
  (expr',changed) <- Writer.listen $ r1 c expr
  if Monoid.getAny changed
    then r2 c expr'
    else return expr'

infixr 5 >-!
-- | Only apply the second transformation if the first one fails.
(>-!) :: Rewrite m -> Rewrite m -> Rewrite m
(>-!) r1 r2 c expr = do
  (expr',changed) <- Writer.listen $ r1 c expr
  if Monoid.getAny changed
    then return expr'
    else r2 c expr'

-- | Keep applying a transformation until it fails.
repeatR :: Rewrite m -> Rewrite m
repeatR r = r !-> repeatR r

whenR :: Monad m
      => ([CoreContext] -> Term -> m Bool)
      -> Transform m
      -> Transform m
whenR f r1 ctx expr = do
  b <- f ctx expr
  if b
    then r1 ctx expr
    else return expr

-- | Only traverse downwards when the assertion evaluates to true
bottomupWhenR :: Fresh m
              => ([CoreContext] -> Term -> m Bool)
              -> Transform m
              -> Transform m
bottomupWhenR f r ctx expr = do
  b <- f ctx expr
  if b
    then (allR True (bottomupWhenR f r) >-> r) ctx expr
    else r ctx expr
