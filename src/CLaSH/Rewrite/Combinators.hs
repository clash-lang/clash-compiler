{-# LANGUAGE ScopedTypeVariables #-}
module CLaSH.Rewrite.Combinators where

import Control.Monad                     ((>=>),(<=<))
import qualified Control.Monad.Writer as Writer
import qualified Data.Monoid          as Monoid
import Unbound.LocallyNameless           (Embed,Fresh,bind,embed,rec,unbind,unembed,unrec)
import Unbound.LocallyNameless.Ops    (unsafeUnbind)

import CLaSH.Core.Term     (Term(..),Pat)
import CLaSH.Core.Util     (patIds)
import CLaSH.Core.Var      (Id)
import CLaSH.Rewrite.Types

allR :: forall m . (Functor m, Monad m, Fresh m) => Bool -> Transform m -> Transform m
allR _ _ _ (Var t x)   = return (Var t x)
allR _ _ _ (Data w dc) = return (Data w dc)
allR _ _ _ (Literal l) = return (Literal l)
allR _ _ _ (Prim p)    = return (Prim p)

allR rf trans c (Lam b) = do
  (v,e) <- if rf then unbind b else return (unsafeUnbind b)
  e'    <- trans ((LamBody v):c) e
  return . Lam $ bind v e'

allR rf trans c (TyLam b) = do
  (tv, e) <- if rf then unbind b else return (unsafeUnbind b)
  e' <- trans ((TyLamBody tv):c) e
  return . TyLam $ bind tv e'

allR _ trans c (App e1 e2) = do
  e1' <- trans (AppFirst:c) e1
  e2' <- trans (AppSecond:c) e2
  return $ App e1' e2'

allR _ trans c (TyApp e ty) = do
  e' <- trans (TyAppC:c) e
  return $ TyApp e' ty

allR rf trans c (Letrec b) = do
  (xesR,e) <- if rf then unbind b else return (unsafeUnbind b)
  let xes   = unrec xesR
  let bndrs = map fst xes
  e' <- trans ((LetBody bndrs):c) e
  xes' <- mapM (rewriteBind bndrs) xes
  return . Letrec $ bind (rec xes') e'
  where
    rewriteBind :: [Id] -> (Id,Embed Term) -> m (Id,Embed Term)
    rewriteBind bndrs (b', e) = do
      e' <- trans ((LetBinding bndrs):c) (unembed e)
      return $ (b',embed e')

allR rf trans c (Case scrut ty alts) = do
  scrut' <- trans (CaseScrut:c) scrut
  alts'  <- if rf then mapM (fmap (uncurry bind) . rewriteAlt <=< unbind) alts
                  else mapM (fmap (uncurry bind) . rewriteAlt . unsafeUnbind) alts
  return $ Case scrut' ty alts'
  where
    rewriteAlt :: (Pat, Term) -> m (Pat, Term)
    rewriteAlt (p,e) = do
      e' <- trans (CaseAlt (patIds p):c) e
      return (p,e')

(>->) :: (Monad m) => Transform m -> Transform m -> Transform m
(>->) r1 r2 c = (r1 c) >=> (r2 c)

topdownR :: (Fresh m, Functor m, Monad m) => Transform m -> Transform m
topdownR r = r >-> allR True (topdownR r)

unsafeTopdownR :: (Fresh m, Functor m, Monad m) => Transform m -> Transform m
unsafeTopdownR r = r >-> allR False (unsafeTopdownR r)

bottomupR :: (Fresh m, Functor m, Monad m) => Transform m -> Transform m
bottomupR r = allR True (bottomupR r) >-> r

unsafeBottomupR :: (Fresh m, Functor m, Monad m) => Transform m -> Transform m
unsafeBottomupR r = allR False (unsafeBottomupR r) >-> r

(!->) :: Monad m => Rewrite m -> Rewrite m -> Rewrite m
(!->) r1 r2 c expr = R $ do
  (expr',changed) <- runR $ Writer.listen $ r1 c expr
  if Monoid.getAny changed
    then runR $ r2 c expr'
    else return expr

repeatR :: Monad m => Rewrite m -> Rewrite m
repeatR r = r !-> (repeatR r)

bottomupProp :: (Fresh m, Functor m, Monad m) => Rewrite m -> Rewrite m
bottomupProp r = bottomupR (r !-> allR True (topdownR r))

unsafeBottomupProp :: (Fresh m, Functor m, Monad m) => Rewrite m -> Rewrite m
unsafeBottomupProp r = unsafeBottomupR (r !-> allR False (unsafeTopdownR r))

-- repeatR r c expr = R $ do
--   (expr',changed) <- runR $ Writer.listen $ r c expr
--   if Monoid.getAny changed
--     then runR $ repeatR r c expr'
--     else return expr
