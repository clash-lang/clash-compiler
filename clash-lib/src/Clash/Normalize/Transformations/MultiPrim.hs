{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.
                    2022-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations on primitives with multiple results, of scoped Clash Core.
  See Note [MultiResult type].
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Transformations.MultiPrim
  ( setupMultiResultPrim
  , setupMultiResultPrimWorker
  ) where

import qualified Control.Lens as Lens
import Data.Text.Extra (showt)
import GHC.Stack (HasCallStack)

import Clash.Core.Basic (IsMultiPrim (..), WorkInfo (..))
import Clash.Annotations.Primitive (extractPrim)
import qualified Clash.Core.Name as C (mkUnsafeSystemName)
import Clash.Core.TyCon (TyConMap)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Primitives.Types (Primitive (..))
import Clash.Rewrite.StrategyDSL (Transformation, onPrim, toTransformation)
import Clash.Rewrite.Types (primitives, tcCache)
import Clash.Util (curLoc)

import Clash.Core.HasType (primInfoType)
import Clash.Core.Subst
  (Subst, idSubst, substScope, substTy, substTyBinder, withFreshIdBinders, withFreshTyBinder)
import Clash.Core.Syntax
import Clash.Core.Term (mkApps, mkLams, mkTmApps, mkTyApps, mkTyLams)
import Clash.Core.TermInfo (MultiPrimInfo (..), multiPrimInfo')
import Clash.Core.Type
  (PiTele (..), SplitFunForall (..), TypeView (..), mkFunTy, splitFunForallTy, tyView)
import Clash.Core.TysPrim (liftedTypeKind)
import Clash.Core.Util (internalName, listToLets)
import Clash.Rewrite (TransformContext, changed, tfScope)

{- Note [MultiResult type]

A multi result primitive assigns its results to multiple result variables
instead of one. Besides producing nicer HDL it works around issues with
synthesis tooling described in:

  https://github.com/clash-lang/clash-compiler/issues/1555

This transformation rewrites primitives indicating they can assign their
results to multiple signals, such that netlist can easily render it. This
involves inserting additional arguments for each of the result values, and
then using the c$multiPrimSelect primitive to select individual results.

Example:

  prim :: forall a b c. a -> (b, c)

will be rewritten to:

  /\a b c -> \(a1 :: a) ->
    letrec
      r  = prim @a @b @c a1 r2 r3 -- With 'MultiResult'
      r2 = c$multiPrimSelect @b @(b, c) r2 r
      r3 = c$multiPrimSelect @c @(b, c) r3 r
    in
      (r2, r3)

Netlist will not render any c$multiPrimSelect primitives. Similar to
primitives having a /void/ return type, r is not rendered either.

c$multiPrimSelect has the closed type forall a b. a -> b -> a: a 'PrimInfo'
isn't indexed by a scope, so the type of a primitive can't mention type
variables of the term it occurs in.

This transformation is currently hardcoded to recognize tuples as return
types, not any product type. It will error if it sees a multi result primitive
with a non-tuple return type.
-}

-- | Rewrite primitives that can assign their results to multiple signals:
-- add an argument for each of the results, which the primitive assigns to,
-- and select the individual results with @c$multiPrimSelect@. Only
-- primitives with a tuple result type are supported. See Note [MultiResult
-- type].
setupMultiResultPrim :: Transformation
setupMultiResultPrim =
  toTransformation "setupMultiResultPrim" (onPrim 'setupMultiResultPrimWorker)

-- | The 'Prim' handler of 'setupMultiResultPrim'
setupMultiResultPrimWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> PrimInfo
  -> NormalizeSession (Term n)
setupMultiResultPrimWorker ctx node pInfo@PrimInfo{primMultiResult = SingleResult} = do
  tcm <- Lens.view tcCache
  prim <- Lens.view (primitives . Lens.at (primName pInfo))

  case prim >>= extractPrim of
    Just (BlackBoxHaskell{multiResult=True}) ->
      changed (setupMultiResultPrim' tcm (tfScope ctx) pInfo)
    Just (BlackBox{multiResult=True}) ->
      changed (setupMultiResultPrim' tcm (tfScope ctx) pInfo)
    _ ->
      return node
setupMultiResultPrimWorker _ctx node _primInfo = return node
{-# SCC setupMultiResultPrimWorker #-}

-- | For @prim :: forall a b c. a -> (b, c)@:
--
-- @
--   /\\a b c -> \\(a1 :: a) ->
--     letrec
--       r  = prim \@a \@b \@c a1 r2 r3 -- With 'MultiResult'
--       r2 = c$multiPrimSelect \@b \@(b, c) r2 r
--       r3 = c$multiPrimSelect \@c \@(b, c) r3 r
--     in
--       (r2, r3)
-- @
setupMultiResultPrim' :: (HasCallStack, Distinct n) => TyConMap -> Scope n -> PrimInfo -> Term n
setupMultiResultPrim' tcm sc pInfo =
  case splitFunForallTy (primInfoType pInfo) of
    SplitFunForall tele res -> openPi (idSubst sc) tele res $ \scT tvs tyVars argTys resTy ->
      let
        resTypes = case tyView resTy of
          TyConApp _ tys -> tys
          _ -> error ($(curLoc) ++ "not a tuple: " ++ show (primType pInfo))
        nArgs = length argTys
        argNms = [internalName ("a" <> showt i) | i <- [1 .. nArgs]]
        resNms = [internalName ("r" <> showt i) | i <- [nArgs + 1 .. nArgs + length resTypes]]
      in
        withFreshIdBinders scT (zip argNms argTys) $ \scA argBs argVs ->
          withFreshIdBinders scA ((internalName "r", sink resTy) : zip resNms (map sink resTypes)) $
            \_ letBs letVs -> case letVs of
              resV : resVs ->
                let
                  resTyL = sink resTy
                  resTypesL = map sink resTypes
                  multiPrimBind =
                    mkApps
                      (Prim (multiResultPrimInfo pInfo))
                      (map (Right . sink) tyVars ++ map Left (map sink argVs ++ resVs))
                  multiPrimSelect r t =
                    mkApps (Prim multiPrimSelectInfo) [Right t, Right resTyL, Left r, Left resV]
                  body = mkTmApps (mkTyApps (Data tupDc) resTypesL) resVs
                in
                  mkTyLams
                    (mkLams
                      (listToLets scA letBs (multiPrimBind : zipWith multiPrimSelect resVs resTypesL) body)
                      argBs)
                    tvs
              [] -> error ($(curLoc) ++ "no binders")
 where
  MultiPrimInfo{mpi_resultDc = tupDc} = multiPrimInfo' tcm pInfo

-- | The primitive with multiple results
multiResultPrimInfo :: PrimInfo -> PrimInfo
multiResultPrimInfo p =
  mkPrimInfo (primName p) (primType p) (primWorkInfo p) MultiResult (primUnfolding p)

-- | The primitive that selects a result of a multi-result primitive, of type
-- @forall a b. a -> b -> a@, see Note [MultiResult type]
multiPrimSelectInfo :: PrimInfo
multiPrimSelectInfo =
  mkPrimInfo "c$multiPrimSelect" selTy WorkAlways SingleResult NoUnfolding
 where
  selTy =
    withFreshTyBinder emptyScope (C.mkUnsafeSystemName "a" 0) liftedTypeKind $ \sc1 a ->
      withFreshTyBinder sc1 (C.mkUnsafeSystemName "b" 1) liftedTypeKind $ \_ b ->
        ForAllTy a (ForAllTy b
          (mkFunTy (sink (tyBinderVar a)) (mkFunTy (tyBinderVar b) (sink (tyBinderVar a)))))

-- | Go under the quantified type variables of a polymorphic function type.
-- Gives their binders, the type variables, the argument types and the
-- result type, in the scope extended with the type variables.
openPi
  :: Distinct o
  => Subst i o
  -> PiTele i l
  -> Type l
  -> (forall o'. DExt o o'
        => Scope o' -> Tele TyBinder o o' -> [Type o'] -> [Type o'] -> Type o' -> r)
  -> r
openPi s tele res k = case tele of
  PiNil -> k (substScope s) TNil [] [] (substTy s res)
  PiArg t rest -> openPi s rest res $ \sc' tvs vs args res' ->
    k sc' tvs vs (sink (substTy s t) : args) res'
  PiForall b rest -> substTyBinder s b $ \s1 b' -> openPi s1 rest res $ \sc' tvs vs args res' ->
    k sc' (TCons b' tvs) (sink (tyBinderVar b') : vs) args res'
