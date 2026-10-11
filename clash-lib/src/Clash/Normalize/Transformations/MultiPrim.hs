{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.
                    2022,2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations on primitives with multiple results.
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Transformations.MultiPrim
  ( setupMultiResultPrim
  , setupMultiResultPrimWorker
  ) where

import qualified Control.Lens as Lens
import qualified Data.Either as Either
import Data.Text.Extra (showt)
import GHC.Stack (HasCallStack)

import Clash.Annotations.Primitive (extractPrim)
import Clash.Core.Name (mkUnsafeInternalName, mkUnsafeSystemName)
import Clash.Core.Term
  ( IsMultiPrim(..), MultiPrimInfo(..), PrimInfo(..), Term(..), WorkInfo(..)
  , mkAbstraction, mkApps, mkTmApps, mkTyApps, PrimUnfolding(..))
import Clash.Core.TermInfo (multiPrimInfo')
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Type (Type(..), mkFunTy, splitFunForallTy)
import Clash.Core.TysPrim (liftedTypeKind)
import Clash.Core.Util (listToLets)
import Clash.Core.Var (mkLocalId, mkTyVar)
import qualified Clash.Core.Scoped.Syntax as S
import qualified Clash.Normalize.Scoped.Transformations.MultiPrim as Scoped
import Clash.Normalize.Types (NormalizeSession)
import Clash.Primitives.Types (Primitive(..))
import Clash.Rewrite.StrategyDSL (Transformation, onPrim, toTransformation)
import Clash.Rewrite.Scoped (ScopedTransform (..))
import qualified Clash.Rewrite.Scoped as Scoped (TransformContext)
import Clash.Rewrite.Scoped.ViaFoil (viaFoilWhen)
import Clash.Rewrite.Types (TransformContext, tcCache, primitives)
import Clash.Rewrite.Util (changed)

-- Note [MultiResult type]
--
-- A multi result primitive assigns its results to multiple result variables
-- instead of one. Besides producing nicer HDL it works around issues with
-- synthesis tooling described in:
--
--   https://github.com/clash-lang/clash-compiler/issues/1555
--
-- This transformation rewrites primitives indicating they can assign their
-- results to multiple signals, such that netlist can easily render it. This
-- involves inserting additional arguments for each of the result values, and
-- then using the c$multiPrimSelect primitive to select individual results.
--
-- Example:
--
-- @
-- prim :: forall a b c. a -> (b, c)
-- @
--
-- will be rewritten to:
--
-- @
--   \\(x :: a) ->
--         let
--            r  = prim @a @b @c x r0 r1 -- With 'Clash.Core.Term.MultiPrim'
--            r0 = c$multiPrimSelect @b @(b, c) r0 r
--            r1 = c$multiPrimSelect @c @(b, c) r1 r
--          in
--            (r0, r1)
-- @
--
-- Netlist will not render any @multiPrimSelect@ primitives. Similar to
-- primitives having a /void/ return type, /r/ is not rendered either.
--
-- @c$multiPrimSelect@ has the closed type @forall a b. a -> b -> a@: the type
-- of a primitive must not mention type variables of the term it occurs in, as
-- substitution doesn't go into it.
--
-- This transformation is currently hardcoded to recognize tuples as return
-- types, not any product type. It will error if it sees a multi result primitive
-- with a non-tuple return type.
--
-- This is the port 'Scoped.setupMultiResultPrim', which only converts the
-- primitives at which it fires, see 'setupMultiResultPrimViaFoil'.
setupMultiResultPrim :: Transformation
setupMultiResultPrim =
  toTransformation "setupMultiResultPrim" (onPrim 'setupMultiResultPrimViaFoil)

-- | The 'Prim' handler of 'setupMultiResultPrim':
-- 'Scoped.setupMultiResultPrimWorker' at the multi-result primitives that
-- aren't set up yet.
--
-- The strategy compiler runs a scoped transformation through 'viaFoil' at
-- every node it is offered at: for 'setupMultiResultPrim', every primitive.
-- This goes away together with 'viaFoil'.
setupMultiResultPrimViaFoil
  :: HasCallStack
  => TransformContext -> Term -> PrimInfo -> NormalizeSession Term
setupMultiResultPrimViaFoil ctx node pInfo =
  viaFoilWhen "setupMultiResultPrim" (\_ _ -> canFire)
    (ScopedTransform setupMultiResultPrimAt) ctx node
 where
  canFire = case pInfo of
    PrimInfo{primMultiResult=SingleResult} -> do
      prim <- Lens.view (primitives . Lens.at (primName pInfo))
      pure $ case prim >>= extractPrim of
        Just (BlackBoxHaskell{multiResult=True}) -> True
        Just (BlackBox{multiResult=True}) -> True
        _ -> False
    _ -> pure False

-- | 'Scoped.setupMultiResultPrimWorker' at a primitive
setupMultiResultPrimAt
  :: (HasCallStack, S.Distinct n)
  => Scoped.TransformContext n -> S.Term n -> NormalizeSession (S.Term n)
setupMultiResultPrimAt ctx e = case e of
  S.Prim p -> Scoped.setupMultiResultPrimWorker ctx e p
  _ -> return e

-- | The 'Prim' handler of 'setupMultiResultPrim'.
setupMultiResultPrimWorker
  :: HasCallStack
  => TransformContext -> Term -> PrimInfo -> NormalizeSession Term
setupMultiResultPrimWorker _ctx node pInfo@PrimInfo{primMultiResult=SingleResult} = do
  tcm <- Lens.view tcCache
  prim <- Lens.view (primitives . Lens.at (primName pInfo))

  case prim >>= extractPrim of
    Just (BlackBoxHaskell{multiResult=True}) ->
      changed (setupMultiResultPrim' tcm pInfo)
    Just (BlackBox{multiResult=True}) ->
      changed (setupMultiResultPrim' tcm pInfo)
    _ ->
      return node
setupMultiResultPrimWorker _ctx node _primInfo = return node

setupMultiResultPrim' :: HasCallStack => TyConMap -> PrimInfo -> Term
setupMultiResultPrim' tcm primInfo@PrimInfo{primType} =
  mkAbstraction letTerm (map Right typeVars <> map Left argIds)
 where
  typeVars = Either.lefts pArgs

  internalNm prefix n = mkUnsafeInternalName (prefix <> showt n) n
  internalId prefix typ n = mkLocalId typ (internalNm prefix n)

  nTermArgs = fromIntegral (length (Either.rights pArgs))
  nResTypes = fromIntegral (length resTypes)
  argIds = zipWith (internalId "a") (Either.rights pArgs) [1..nTermArgs]
  resIds = zipWith (internalId "r") resTypes [nTermArgs+1..nTermArgs+nResTypes]
  resId = mkLocalId pResTy (mkUnsafeInternalName "r" (nTermArgs+nResTypes+1))

  (pArgs, pResTy) = splitFunForallTy primType
  MultiPrimInfo{mpi_resultDc=tupTc, mpi_resultTypes=resTypes} =
    multiPrimInfo' tcm primInfo

  multiPrimSelect r t =
    (r, mkApps (Prim multiPrimSelectInfo) [Right t, Right pResTy, Left (Var r), Left (Var resId)])
  multiPrimSelectBinds = zipWith multiPrimSelect  resIds resTypes
  multiPrimTermArgs = map (Left . Var) (argIds <> resIds)
  multiPrimTypeArgs = map (Right . VarTy) typeVars
  multiPrimBind =
    mkApps
      (Prim primInfo{primMultiResult=MultiResult})
      (multiPrimTypeArgs <> multiPrimTermArgs)

  -- forall a b. a -> b -> a
  multiPrimSelectInfo = PrimInfo
    { primName = "c$multiPrimSelect"
    , primType =
        ForAllTy selA (ForAllTy selB (mkFunTy (VarTy selA) (mkFunTy (VarTy selB) (VarTy selA))))
    , primWorkInfo = WorkAlways
    , primMultiResult = SingleResult
    , primUnfolding = NoUnfolding
    }

  selA = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 0)
  selB = mkTyVar liftedTypeKind (mkUnsafeSystemName "b" 1)

  letTerm =
    listToLets
      ((resId,multiPrimBind):multiPrimSelectBinds)
      (mkTmApps (mkTyApps (Data tupTc) resTypes) (map Var resIds))
