{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Transformations on primitives with multiple results.
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Normalize.Transformations.MultiPrim
  ( setupMultiResultPrim
  ) where

import qualified Control.Lens as Lens
import qualified Data.Either as Either
import Data.Text.Extra (showt)
import GHC.Stack (HasCallStack)

import Clash.Annotations.Primitive (extractPrim)
import Clash.Core.Name (mkUnsafeInternalName)
import Clash.Core.Term
  ( IsMultiPrim(..), MultiPrimInfo(..), PrimInfo(..), Term(..), WorkInfo(..)
  , mkAbstraction, mkApps, mkTmApps, mkTyApps, PrimUnfolding(..))
import Clash.Core.TermInfo (multiPrimInfo')
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Type (Type(..), mkPolyFunTy, splitFunForallTy)
import Clash.Core.Util (listToLets)
import Clash.Core.Var (mkLocalId)
import Clash.Normalize.Types (NormRewrite, primitives)
import Clash.Primitives.Types (Primitive(..))
import Clash.Rewrite.Types (extra, tcCache)
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
--   \(x :: a) ->
--         let
--            r  = prim @a @b @c x r0 r1 -- With 'Clash.Core.Term.MultiPrim'
--            r0 = c$multiPrimSelect r0 r
--            r1 = c$multiPrimSelect r1 r
--          in
--            (r0, r1)
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
    , primMultiResult = SingleResult
    , primUnfolding = NoUnfolding
    }

  letTerm =
    listToLets
      ((resId,multiPrimBind):multiPrimSelectBinds)
      (mkTmApps (mkTyApps (Data tupTc) resTypes) (map Var resIds))
