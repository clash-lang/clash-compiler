{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The eta-expansion transformations of Clash Core.

  'etaExpansionTL' moves the lambdas in the body of a let-expression out of
  it. It creates new binders for them in the scope of the let-expression, so
  they can't capture the free variables of the let-bindings, and the
  let-binders can't capture references to them.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Transformations.EtaExpand
  ( etaExpandSyn
  , etaExpandSynWorker
  , etaExpansionTL
  ) where

import qualified Control.Lens as Lens
import GHC.Stack (HasCallStack)

import qualified Clash.Core.Name as C

import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.StrategyDSL
  ( Transformation, onAppNode, onTickNode, onTyAppNode, onVarNode
  , toTransformation )
import Clash.Rewrite.Types (typeEnv)
import Clash.Util (curLoc)

import Clash.Core.HasType (inferCoreTypeOf)
import Clash.Core.Subst
  ( extendSubstScope, extendTmSubstTele, idSubst, strengthenTyId
  , strengthenTyIds, substBind, substTm, teleIdBinderTypes, withFreshIdBinder
  , withFreshIdBinders )
import Clash.Core.Syntax
import Clash.Core.Term (collectArgs, mkLams)
import Clash.Core.Type (splitFunTy)
import Clash.Core.Util (internalName)
import Clash.Rewrite
  ( FrameKind (KAppFun), TransformContext, Under (LamBody), changed, ctxFrames
  , isTickKind, pushUnder, tfScope, underLam, underLet )
import Clash.Rewrite.Globals (isTopEntity)

-- | Eta-expand functions with a Synthesize annotation, needed to allow such
-- functions to appear as arguments to higher-order primitives.
etaExpandSyn :: Transformation
etaExpandSyn =
  toTransformation "etaExpandSyn"
    (onVarNode 'etaExpandSynWorker <> onAppNode 'etaExpandSynWorker
      <> onTyAppNode 'etaExpandSynWorker <> onTickNode 'etaExpandSynWorker)

-- | The application-spine handler of 'etaExpandSyn'
etaExpandSynWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (Term n)
etaExpandSynWorker ctx e
  | (GlobalVar g, _) <- collectArgs e
  , not (isAppFunCtx (ctxFrames ctx)) = do
      isTopEnt <- isTopEntity g
      te <- Lens.view typeEnv
      case splitFunTy te sc (inferCoreTypeOf te sc e) of
        Just (argTy, _) | isTopEnt ->
          withFreshIdBinder sc (internalName "arg") argTy $ \_ b ->
            changed (Lam b (App (sink e) (idBinderVar b)))
        _ -> return e
  | otherwise = return e
 where
  sc = tfScope ctx

  isAppFunCtx = \case
    KAppFun : _ -> True
    k : c | isTickKind k -> isAppFunCtx c
    _ -> False
{-# SCC etaExpandSynWorker #-}

-- | Eta-expand top-level lambdas (DON'T use in a traversal!): eta-expands the
-- body under the lambdas and let-expressions at the top of a term, if it has
-- a function type, and moves lambdas out of the body of a let-expression
etaExpansionTL
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (Term n)
etaExpansionTL ctx = \case
  Lam b e -> underLam ctx b e $ \ctx' b' e' -> Lam b' <$> etaExpansionTL ctx' e'
  Let bind e -> underLet ctx bind e $ \ctx' bind' e' -> do
    e'' <- etaExpansionTL ctx' e'
    case e'' of
      Lam {} -> changed (floatLams (tfScope ctx) bind' e'')
      _ -> return (Let bind' e'')
  e -> do
    te <- Lens.view typeEnv
    let sc = tfScope ctx
    case splitFunTy te sc (inferCoreTypeOf te sc e) of
      Just (argTy, _) ->
        withFreshIdBinder sc (internalName "arg") argTy $ \sc' b -> do
          e' <- etaExpansionTL (pushUnder sc' (LamBody b) ctx) (App (sink e) (idBinderVar b))
          changed (Lam b e')
      Nothing -> return e
{-# SCC etaExpansionTL #-}

-- | Move the lambdas at the head of the body of a let-expression out of it:
--
-- @
-- let bind in \\y1 .. yk -> e  ==>  \\y1' .. yk' -> let bind in e[y1'/y1, ..]
-- @
--
-- The new lambda binders are fresh in the scope of the let-expression, and
-- its binders are renamed if they clash with them.
floatLams :: (HasCallStack, Distinct n) => Scope n -> Bind n l -> Term l -> Term n
floatLams sc bind e0 = collectLams e0 $ \ys e ->
  let tys = map (strengthenBind bind) (teleIdBinderTypes ys)
  in  withFreshIdBinders sc (zip (teleOccs ys) tys) $ \sc' ys' vs' ->
        substBind (extendSubstScope sc' (idSubst sc)) bind $ \s bind' ->
          case extendTmSubstTele s ys (map sink vs') of
            Just s' -> mkLams (Let bind' (substTm s' e)) ys'
            Nothing -> error ($(curLoc) ++ "floatLams: binders and variables don't match")
 where
  -- Types don't mention term variables
  strengthenBind :: Bind x y -> Type y -> Type x
  strengthenBind = \case
    NonRec b _ -> strengthenTyId b
    Rec bs _ -> strengthenTyIds bs

  teleOccs :: Tele IdBinder x y -> [C.Name Term]
  teleOccs = \case
    TNil -> []
    TCons b rest -> idBinderOcc b : teleOccs rest

-- | The binders of the lambdas at the head of a term, and its body under them
collectLams :: Term n -> (forall l. Tele IdBinder n l -> Term l -> r) -> r
collectLams e k = case e of
  Lam b body -> collectLams body $ \bs body' -> k (TCons b bs) body'
  _ -> k TNil e
