{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Conversion from scoped Clash Core ("Clash.Core.Scoped.Syntax") to Clash Core
  ("Clash.Core.Term") that gives every binder a fresh unique from a unique
  supply, made unique in an 'InScopeSet'.

  'Clash.Core.Scoped.Convert.toClashTerm' uses the raw names of binders as
  uniques. Raw names are small numbers, just above those in scope, like the
  ones the unique supply hands out. A converted binder could then share its
  unique with a variable that the supply hands out later, e.g. a new global
  binder, whose freshness is only checked against the global bindings. This
  conversion avoids that, e.g. for the normalized bindings that Netlist gets
  ('Clash.Rewrite.Scoped.Globals.toClashBindings').
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Clash.Core.Scoped.Convert.Fresh
  ( BackEnv (..)
  , emptyBackEnv
  , toClashTermFreshClosed
  , toClashFresh
  , toClashTypeFresh
  , toClashTickFresh
  , freshIdBinder
  , freshTyBinder
  , freshTele
  , freshPat
  ) where

import           Control.Monad.State.Strict  (State, state)
import           Data.IntMap.Strict          (IntMap)
import qualified Data.IntMap.Strict          as IntMap
import           GHC.Stack                   (HasCallStack)

import qualified Clash.Core.Name             as C
import qualified Clash.Core.Term             as C
import qualified Clash.Core.Type             as C
import qualified Clash.Core.Var              as C
import           Clash.Core.VarEnv
  (InScopeSet, emptyInScopeSet, extendInScopeSet, uniqAway)
import           Clash.Unique                (Unique)
import           Clash.Util                  (curLoc)
import           Clash.Util.Supply           (Supply, freshId)

import           Clash.Core.Scoped.Syntax

-- | Clash variables of the names of a scope, by raw name
data BackEnv = BackEnv !(IntMap C.TyVar) !(IntMap C.Id)

-- | The environment of a closed term
emptyBackEnv :: BackEnv
emptyBackEnv = BackEnv IntMap.empty IntMap.empty

-- | A Clash variable for a binder: one with a fresh unique from the supply,
-- made unique in the in-scope set, which is extended with it
freshVar :: InScopeSet -> C.Var a -> State Supply (InScopeSet, C.Var a)
freshVar is v = do
  u <- state freshId
  let v' = uniqAway is (setUniq v u)
  pure (extendInScopeSet is v', v')
 where
  setUniq :: C.Var a -> Unique -> C.Var a
  setUniq var u = var {C.varUniq = u, C.varName = (C.varName var) {C.nameUniq = u}}

freshIdBinder
  :: InScopeSet -> BackEnv -> IdBinder n l
  -> State Supply (InScopeSet, BackEnv, C.Id)
freshIdBinder is env@(BackEnv tvs ids) b = do
  ty <- toClashTypeFresh is env (idBinderType b)
  (is', v) <- freshVar is (C.mkLocalId ty (idBinderOcc b))
  pure (is', BackEnv tvs (IntMap.insert (tmNameRaw (tmNameOf b)) v ids), v)

freshTyBinder
  :: InScopeSet -> BackEnv -> TyBinder n l
  -> State Supply (InScopeSet, BackEnv, C.TyVar)
freshTyBinder is env@(BackEnv tvs ids) b = do
  k <- toClashTypeFresh is env (tyBinderKind b)
  (is', v) <- freshVar is (C.mkTyVar k (tyBinderOcc b))
  pure (is', BackEnv (IntMap.insert (tyNameRaw (tyNameOf b)) v tvs) ids, v)

freshTele
  :: (forall x y. InScopeSet -> BackEnv -> b x y -> State Supply (InScopeSet, BackEnv, v))
  -> InScopeSet -> BackEnv -> Tele b n l
  -> State Supply (InScopeSet, BackEnv, [v])
freshTele _ is env TNil = pure (is, env, [])
freshTele f is env (TCons b bs) = do
  (is1, env1, v) <- f is env b
  (is2, env2, vs) <- freshTele f is1 env1 bs
  pure (is2, env2, v : vs)

freshPat
  :: InScopeSet -> BackEnv -> Pat n l
  -> State Supply (InScopeSet, BackEnv, C.Pat)
freshPat is env = \case
  DataPat dc tvs ids -> do
    (is1, env1, tvs') <- freshTele freshTyBinder is env tvs
    (is2, env2, ids') <- freshTele freshIdBinder is1 env1 ids
    pure (is2, env2, C.DataPat dc tvs' ids')
  LitPat l -> pure (is, env, C.LitPat l)
  DefaultPat -> pure (is, env, C.DefaultPat)

-- | Convert a type to Clash Core, see 'toClashFresh'
toClashTypeFresh :: InScopeSet -> BackEnv -> Type n -> State Supply C.Type
toClashTypeFresh is env@(BackEnv tvs _) = \case
  VarTy occ _ n -> case IntMap.lookup (tyNameRaw n) tvs of
    Just tv -> pure (C.VarTy tv)
    Nothing -> error ($(curLoc) ++ "type variable not in scope: " ++ show occ)
  ConstTy c -> pure (C.ConstTy c)
  ForAllTy b t -> do
    (is', env', tv) <- freshTyBinder is env b
    C.ForAllTy tv <$> toClashTypeFresh is' env' t
  AppTy t1 t2 -> C.AppTy <$> toClashTypeFresh is env t1 <*> toClashTypeFresh is env t2
  LitTy l -> pure (C.LitTy l)
  AnnType a t -> C.AnnType a <$> toClashTypeFresh is env t

-- | Convert a term to Clash Core, giving every binder a fresh unique, see the
-- module header. The free variables of the term must be in the environment.
toClashFresh :: HasCallStack => InScopeSet -> BackEnv -> Term n -> State Supply C.Term
toClashFresh is env@(BackEnv _ ids) = \case
  Var occ _ n -> case IntMap.lookup (tmNameRaw n) ids of
    Just v -> pure (C.Var v)
    Nothing -> error ($(curLoc) ++ "variable not in scope: " ++ show occ)
  GlobalVar g -> pure (C.Var (globalIdVar g))
  Data dc -> pure (C.Data dc)
  Literal l -> pure (C.Literal l)
  Prim p -> pure (C.Prim p)
  Lam b e -> do
    (is', env', v) <- freshIdBinder is env b
    C.Lam v <$> toClashFresh is' env' e
  TyLam b e -> do
    (is', env', tv) <- freshTyBinder is env b
    C.TyLam tv <$> toClashFresh is' env' e
  App e1 e2 -> C.App <$> go e1 <*> go e2
  TyApp e ty -> C.TyApp <$> go e <*> goTy ty
  Let (NonRec b x) e -> do
    x' <- go x
    (is', env', v) <- freshIdBinder is env b
    C.Let (C.NonRec v x') <$> toClashFresh is' env' e
  Let (Rec bs xs) e -> do
    (is', env', vs) <- freshTele freshIdBinder is env bs
    xs' <- mapM (toClashFresh is' env') xs
    e' <- toClashFresh is' env' e
    pure (C.Let (C.Rec (zip vs xs')) e')
  Case e ty alts -> C.Case <$> go e <*> goTy ty <*> mapM goAlt alts
  Cast e ty1 ty2 -> C.Cast <$> go e <*> goTy ty1 <*> goTy ty2
  Tick t e -> C.Tick <$> toClashTickFresh is env t <*> go e
 where
  go :: Term x -> State Supply C.Term
  go = toClashFresh is env

  goTy :: Type x -> State Supply C.Type
  goTy = toClashTypeFresh is env

  goAlt :: Alt x -> State Supply C.Alt
  goAlt (Alt p e) = do
    (is', env', p') <- freshPat is env p
    (p',) <$> toClashFresh is' env' e

toClashTickFresh :: InScopeSet -> BackEnv -> TickInfo n -> State Supply C.TickInfo
toClashTickFresh is env = \case
  SrcSpan s -> pure (C.SrcSpan s)
  NameMod m ty -> C.NameMod m <$> toClashTypeFresh is env ty
  DeDup -> pure C.DeDup
  NoDeDup -> pure C.NoDeDup
  Attributes ty e -> C.Attributes <$> toClashTypeFresh is env ty <*> toClashFresh is env e

-- | Convert a closed term to Clash Core, giving every binder a fresh unique
-- from the unique supply, see the module header
toClashTermFreshClosed :: HasCallStack => Term 'VoidS -> State Supply C.Term
toClashTermFreshClosed = toClashFresh emptyInScopeSet emptyBackEnv
