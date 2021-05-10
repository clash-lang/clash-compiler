{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Transformations of the Normalization process
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Transformations
  ( module X
  , etaExpansionTL
  , etaExpandSyn
  , separateArguments
  , separateLambda
  , setupMultiResultPrim
  )
where

import qualified Control.Lens                as Lens
import           Control.Monad.Writer        (listen)
import qualified Data.Either                 as Either
import qualified Data.List                   as List
import qualified Data.Maybe                  as Maybe
import qualified Data.Monoid                 as Monoid
import           Data.Text.Extra             (showt)
import           GHC.Stack                   (HasCallStack)

import           Clash.Annotations.Primitive (extractPrim)
import           Clash.Core.Name (mkUnsafeInternalName, Name (..))
import           Clash.Core.Subst
import           Clash.Core.Term
import           Clash.Core.TermInfo
import           Clash.Core.Type             (Type (..),
                                              splitFunForallTy, splitFunTy,
                                              mkPolyFunTy)
import           Clash.Core.TyCon            (TyConMap)
import           Clash.Core.Util (Projections (..), shouldSplit, mkInternalVar)
import           Clash.Core.Var
  (Id, TyVar, Var (..), isGlobalId, mkLocalId)
import           Clash.Core.VarEnv
  (elemVarSet, extendInScopeSet, extendInScopeSetList, uniqAway)
import Clash.Normalize.Transformations.ANF as X
import Clash.Normalize.Transformations.Case as X
import Clash.Normalize.Transformations.Cast as X
import Clash.Normalize.Transformations.DEC as X
import Clash.Normalize.Transformations.Inline as X
import Clash.Normalize.Transformations.Letrec as X
import Clash.Normalize.Transformations.Reduce as X
import Clash.Normalize.Transformations.Specialize as X
import Clash.Normalize.Transformations.XOptimize as X
import           Clash.Normalize.Types
import           Clash.Primitives.Types (Primitive(..))
import           Clash.Rewrite.Types
import           Clash.Rewrite.Util
import           Clash.Util

-- | Eta-expand top-level lambda's (DON'T use in a traversal!)
etaExpansionTL :: HasCallStack => NormRewrite
etaExpansionTL (TransformContext is0 ctx) (Lam bndr e) = do
  e' <- etaExpansionTL
          (TransformContext (extendInScopeSet is0 bndr) (LamBody bndr:ctx))
          e
  return $ Lam bndr e'

etaExpansionTL (TransformContext is0 ctx) (Letrec xes e) = do
  let bndrs = map fst xes
  e' <- etaExpansionTL
          (TransformContext (extendInScopeSetList is0 bndrs)
                            (LetBody bndrs:ctx))
          e
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
        e' <- etaExpansionTL (TransformContext (extendInScopeSet is0 newId)
                                               (LamBody newId:ctx))
                             (App e (Var newId))
        changed (Lam newId e')
      else return e
{-# SCC etaExpansionTL #-}

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


-- | Worker function of 'separateArguments'.
separateLambda
  :: TyConMap
  -> TransformContext
  -> Id
  -- ^ Lambda binder
  -> Term
  -- ^ Lambda body
  -> Maybe Term
  -- ^ If lambda is split up, this function returns a Just containing the new term
separateLambda tcm ctx@(TransformContext is0 _) b eb0 =
  case shouldSplit tcm (varType b) of
    Just (dc, _, argTys) ->
      let
        nm = mkDerivedName ctx (nameOcc (varName b))
        bs0 = map (`mkLocalId` nm) argTys
        (is1, bs1) = List.mapAccumL newBinder is0 bs0
        subst = extendIdSubst (mkSubst is1) b (dc (map Var bs1))
        eb1 = substTm "separateArguments" subst eb0
      in
        Just (mkLams eb1 bs1)
    _ ->
      Nothing
 where
  newBinder isN0 x =
    let
      x' = uniqAway isN0 x
      isN1 = extendInScopeSet isN0 x'
    in
      (isN1, x')
{-# SCC separateLambda #-}

-- | Split apart (global) function arguments that contain types that we
-- want to separate off, e.g. Clocks. Works on both the definition side (i.e. the
-- lambda), and the call site (i.e. the application of the global variable). e.g.
-- turns
--
-- > f :: (Clock System, Reset System) -> Signal System Int
--
-- into
--
-- > f :: Clock System -> Reset System -> Signal System Int
separateArguments :: HasCallStack => NormRewrite
separateArguments ctx e0@(Lam b eb) = do
  tcm <- Lens.view tcCache
  case separateLambda tcm ctx b eb of
    Just e1 -> changed e1
    Nothing -> return e0

separateArguments (TransformContext is0 _) e@(collectArgsTicks -> (Var g, args, ticks))
  | isGlobalId g = do
  -- We ensure that both the type of the global variable reference is updated
  -- to take into account the changed arguments, and that we apply the global
  -- function with the split apart arguments.
  let (argTys0,resTy) = splitFunForallTy (varType g)
  (concat -> args1, Monoid.getAny -> hasChanged)
    <- listen (mapM (uncurry splitArg) (zip argTys0 args))
  if hasChanged then
    let (argTys1,args2) = unzip args1
        gTy = mkPolyFunTy resTy argTys1
    in  return (mkApps (mkTicks (Var g {varType = gTy}) ticks) args2)
  else
    return e

 where
  -- Split a single argument
  splitArg
    :: Either TyVar Type
    -- The quantifier/function argument type of the global variable
    -> Either Term Type
    -- The applied type argument or term argument
    -> NormalizeSession [(Either TyVar Type,Either Term Type)]
  splitArg tv arg@(Right _)    = return [(tv,arg)]
  splitArg ty arg@(Left tmArg) = do
    tcm <- Lens.view tcCache
    let argTy = termType tcm tmArg
    case shouldSplit tcm argTy of
      Just (_,Projections projections,_) -> do
        tmArgs <- projections is0 tmArg
        changed (map ((ty,) . Left) tmArgs)
      _ ->
        return [(ty,arg)]

separateArguments _ e = return e
{-# SCC separateArguments #-}

-- A multi result primitive assigns its results to multiple result variables
-- instead of one. Besides producing nicer HDL it works around issues with
-- synthesis tooling described in:
--
--   https://github.com/clash-lang/clash-compiler/issues/1555
--
-- This transformation rewrites primitives indicating they can assign their
-- results to multiple signals, such that netlist can easily render it.
--
-- Example:
--
-- @
-- prim :: forall a. a -> (a, a)
-- @
--
-- will be rewritten to:
--
-- @
--   \a0 -> let
--            r  = prim @t0 a0 r0 r1     -- With 'Clash.Core.Term.MultiPrim'
--            r0 = multiPrimSelect r0 r
--            r1 = multiPrimSelect r1 r
--          in
--            (x, y)
-- @
--
-- Netlist will not render any @multiPrimSelect@ primitives. Similar to
-- primitives having a /void/ return type, /r/ is not rendered either.
--
-- This transformation is currently hardcoded to recognize tuples as return
-- types, not any product type. It will error if it sees a multi result primitive
-- with a non-tuple return type.
--
setupMultiResultPrim :: HasCallStack => NormRewrite
setupMultiResultPrim _ctx e@(Prim pInfo@PrimInfo{primMultiResult=SingleResult}) = do
  tcm <- Lens.view tcCache
  prim <- Lens.use (extra . primitives . Lens.at (primName pInfo))

  case prim >>= extractPrim of
    Just (BlackBoxHaskell{multiResult=True}) ->
      changed (setupMultiResultPrim' tcm pInfo)
    Just (BlackBox{multiResult=True}) ->
      changed (setupMultiResultPrim' tcm pInfo)
    _ ->
      return e

setupMultiResultPrim _ e = return e

setupMultiResultPrim' :: HasCallStack => TyConMap -> PrimInfo -> Term
setupMultiResultPrim' tcm primInfo@PrimInfo{primType} =
  mkAbstraction letTerm (map Right typeVars <> map Left argIds)
 where
  typeVars = Either.lefts pArgs

  internalNm prefix n = mkUnsafeInternalName (prefix <> showt n) n
  internalId prefix typ n = mkLocalId typ (internalNm prefix n)

  nTermArgs = length (Either.rights pArgs)
  argIds = zipWith (internalId "a") (Either.rights pArgs) [1..nTermArgs]
  resIds = zipWith (internalId "r") resTypes [nTermArgs+1..nTermArgs+length resTypes]
  resId = mkLocalId pResTy (mkUnsafeInternalName "r" (nTermArgs+length resTypes+1))

  (pArgs, pResTy) = splitFunForallTy primType
  MultiPrimInfo{mpi_resultDc=tupTc, mpi_resultTypes=resTypes} =
    multiPrimInfo' tcm primInfo

  multiPrimSelect r t = (r, mkTmApps (Prim (multiPrimSelectInfo t)) [Var r, Var resId])
  multiPrimSelectBinds = zipWith multiPrimSelect  resIds resTypes
  multiPrimTermArgs = map (Left . Var) (argIds <> resIds)
  multiPrimTypeArgs = map (Right . VarTy) typeVars
  multiPrimBind =
    mkApps
      (Prim primInfo{primMultiResult=MultiResult})
      (multiPrimTypeArgs <> multiPrimTermArgs)

  multiPrimSelectInfo t = PrimInfo
    { primName = "c$multiPrimSelect"
    , primType = mkPolyFunTy pResTy [Right pResTy, Right t]
    , primWorkInfo = WorkAlways
    , primMultiResult = SingleResult }

  letTerm =
    Letrec
      ((resId,multiPrimBind):multiPrimSelectBinds)
      (mkTmApps (mkTyApps (Data tupTc) resTypes) (map Var resIds))


