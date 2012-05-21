{-# LANGUAGE ScopedTypeVariables #-}
module CLaSH.Rewrite.Combinators where

import Control.Monad                     ((<=<),(>=>))
import qualified Control.Monad.Writer as Writer
import qualified Data.Monoid          as Monoid
import Unbound.LocallyNameless           (Embed,bind,embed,rec,unbind,unembed,unrec)

import CLaSH.Core.Term     (Term(..),Pat)
import CLaSH.Core.Util     (patIds)
import CLaSH.Core.Var      (Id)
import CLaSH.Rewrite.Types

allR :: forall m . (Functor m, Monad m) => Rewrite m -> Rewrite m
allR _ _ (Var x)     = return (Var x)
allR _ _ (Data dc)   = return (Data dc)
allR _ _ (Literal l) = return (Literal l)
allR _ _ (Prim p)    = return (Prim p)

allR trans c (Lam b) = R $ do
  (v,e) <- unbind b
  e'    <- runR $ trans ((LamBody v):c) e
  return . Lam $ bind v e'

allR trans c (TyLam b)   = R $ do
  (tv, e) <- unbind b
  e' <- runR $ trans ((TyLamBody tv):c) e
  return . TyLam $ bind tv e'

allR trans c (App e1 e2) = R $ do
  e1' <- runR $ trans (AppFirst:c) e1
  e2' <- runR $ trans (AppSecond:c) e2
  return $ App e1' e2'

allR trans c (TyApp e ty) = R $ do
  e' <- runR $ trans (TyAppC:c) e
  return $ TyApp e' ty

allR trans c (Letrec b) = R $ do
  (xesR,e) <- unbind b
  let xes   = unrec xesR
  let bndrs = map fst xes
  e' <- runR $ trans ((LetBody bndrs):c) e
  xes' <- runR $ mapM (rewriteBind bndrs) xes
  return . Letrec $ bind (rec xes') e'
  where
    rewriteBind :: [Id] -> (Id,Embed Term) -> R m (Id,Embed Term)
    rewriteBind bndrs (b', e) = R $ do
      e' <- runR $ trans ((LetBinding bndrs):c) (unembed e)
      return $ (b',embed e')

allR trans c (Case scrut ty alts) = R $ do
  scrut' <- runR $ trans (CaseScrut:c) scrut
  alts'  <- runR $ mapM (fmap (uncurry bind) . rewriteAlt <=< unbind) alts
  return $ Case scrut' ty alts'
  where
    rewriteAlt :: (Pat, Term) -> R m (Pat, Term)
    rewriteAlt (p,e) = R $ do
      e' <- runR $ trans (CaseAlt (patIds p):c) e
      return (p,e')

(>->) :: Monad m => Rewrite m -> Rewrite m -> Rewrite m
(>->) r1 r2 c = (r1 c) >=> (r2 c)

topdownR :: (Functor m, Monad m) => Rewrite m -> Rewrite m
topdownR r = r >-> allR (topdownR r)

bottomupR :: (Functor m, Monad m) => Rewrite m -> Rewrite m
bottomupR r = allR (bottomupR r) >-> r

repeatR :: Monad m => Rewrite m -> Rewrite m
repeatR r c expr = R $ do
  (expr',changed) <- runR $ Writer.listen $ r c expr
  if Monoid.getAny changed
    then runR $ repeatR r c expr'
    else return expr
