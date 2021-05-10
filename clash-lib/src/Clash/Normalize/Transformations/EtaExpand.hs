{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021     , QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The eta-expansion transformation.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Transformations.EtaExpand
  ( etaExpandSyn
  , etaExpansionTL
  ) where

import qualified Control.Lens as Lens
import qualified Data.Maybe as Maybe
import GHC.Stack (HasCallStack)

import Clash.Core.Term (CoreContext(..), Term(..), collectArgs, mkLams)
import Clash.Core.TermInfo (isFun, termType)
import Clash.Core.Type (splitFunTy)
import Clash.Core.Util (mkInternalVar)
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (elemVarSet, extendInScopeSet, extendInScopeSetList)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.Types (TransformContext(..), tcCache, topEntities)
import Clash.Rewrite.Util (changed)
import Clash.Util (curLoc)

-- | Eta-expand functions with a Synthesize annotation, needed to allow such
-- functions to appear as arguments to higher-order primitives.
etaExpandSyn :: HasCallStack => NormRewrite
etaExpandSyn (TransformContext is0 ctx) e@(collectArgs -> (Var f, _)) = do
  topEnts <- Lens.view topEntities
  tcm <- Lens.view tcCache
  let isTopEnt = f `elemVarSet` topEnts
      isAppFunCtx =
        \case
          AppFun:_ -> True
          TickC _:c -> isAppFunCtx c
          _ -> False
      argTyM = fmap fst (splitFunTy tcm (termType tcm e))
  case argTyM of
    Just argTy | isTopEnt && not (isAppFunCtx ctx) -> do
      newId <- mkInternalVar is0 "arg" argTy
      changed (Lam newId (App e (Var newId)))
    _ -> return e

etaExpandSyn _ e = return e
{-# SCC etaExpandSyn #-}

-- | Eta-expand top-level lambda's (DON'T use in a traversal!)
etaExpansionTL :: HasCallStack => NormRewrite
etaExpansionTL (TransformContext is0 ctx) (Lam bndr e) = do
  let ctx' = TransformContext (extendInScopeSet is0 bndr) (LamBody bndr : ctx)
  e' <- etaExpansionTL ctx' e
  return $ Lam bndr e'

etaExpansionTL (TransformContext is0 ctx) (Letrec xes e) = do
  let bndrs = map fst xes
      ctx' = TransformContext (extendInScopeSetList is0 bndrs) (LetBody bndrs : ctx)
  e' <- etaExpansionTL ctx' e
  case stripLambda e' of
    (bs@(_:_),e2) -> do
      let e3 = Letrec xes e2
      changed (mkLams e3 bs)
    _ -> return (Letrec xes e')
  where
    stripLambda :: Term -> ([Id],Term)
    stripLambda (Lam bndr e0) =
      let (bndrs,e1) = stripLambda e0
      in  (bndr:bndrs,e1)
    stripLambda e' = ([],e')

etaExpansionTL (TransformContext is0 ctx) e
  = do
    tcm <- Lens.view tcCache
    if isFun tcm e
      then do
        let argTy = ( fst
                    . Maybe.fromMaybe (error $ $(curLoc) ++ "etaExpansion splitFunTy")
                    . splitFunTy tcm
                    . termType tcm
                    ) e
        newId <- mkInternalVar is0 "arg" argTy
        let ctx' = TransformContext (extendInScopeSet is0 newId) (LamBody newId : ctx)
        e' <- etaExpansionTL ctx' (App e (Var newId))
        changed (Lam newId e')
      else return e
{-# SCC etaExpansionTL #-}
