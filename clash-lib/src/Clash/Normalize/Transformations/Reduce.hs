{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2022, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations for compile-time reduction of expressions / primitives.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Normalize.Transformations.Reduce
  ( reduceBinders
  , reduceConst
  , reduceNonRepPrim
  ) where

import qualified Control.Lens as Lens
import Control.Monad.Trans.Except (runExcept)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Maybe as Maybe
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Stack (HasCallStack)

import Clash.Core.FreeVars (typeFreeVars)
import Clash.Core.HasType
import Clash.Core.Name (nameOcc)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Subst (Subst, extendIdSubst, substTm)
import Clash.Core.Term
  ( CoreContext(..), LetBinding, PrimInfo(..), Term(..), TickInfo(..), collectArgs
  , collectArgsTicks, mkApps, mkTicks, mkTmApps)
import Clash.Core.TyCon (tyConDataCons)
import Clash.Core.Type (Type, TypeView(..), mkTyConApp, splitFunForallTy, tyView)
import Clash.Core.Util (mkVec, shouldSplit, tyNatSize, mkInternalVar)
import Clash.Core.VarEnv (extendInScopeSet)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Normalize.PrimitiveReductions
import Clash.Normalize.Primitives (removedArg)
import Clash.Normalize.Types (NormRewrite, NormalizeSession)
import Clash.Normalize.Util (shouldReduce)
import Clash.Rewrite.Types (TransformContext(..), tcCache, normalizeUltra)
import Clash.Rewrite.Util (changed, isUntranslatableType, setChanged, whnfRW)

-- | XXX: is given inverse topologically sorted binders, but returns
-- topologically sorted binders
--
-- TODO: check further speed improvements:
--
-- 1. Store the processed binders in a `Map Expr LetBinding`:
--    * Trades O(1) `cons` and O(n)*aeqTerm `find` for:
--    * O(log n)*aeqTerm `insert` and O(log n)*aeqTerm `lookup`
-- 2. Store the processed binders in a `AEQTrie Expr LetBinding`
--    * Trades O(1) `cons` and O(n)*aeqTerm `find` for:
--    * O(e) `insert` and O(e) `lookup`
reduceBinders
  :: Subst
  -> [LetBinding]
  -> [LetBinding]
  -> NormalizeSession (Subst, [LetBinding])
reduceBinders !subst processed [] = return (subst,processed)
reduceBinders !subst processed ((i,substTm "reduceBinders" subst -> e):rest)
  | (_,_,ticks) <- collectArgsTicks e
  , NoDeDup `notElem` ticks
  , Just (i1,_) <- List.find ((== e) . snd) processed
  = do
    let subst1 = extendIdSubst subst i (Var i1)
    setChanged
    reduceBinders subst1 processed rest
  | otherwise
  = reduceBinders subst ((i,e):processed) rest
{-# SCC reduceBinders #-}

reduceConst :: HasCallStack => NormRewrite
reduceConst ctx e@(App _ _)
  | (Prim p0, _) <- collectArgs e
  = whnfRW False ctx e $ \_ctx1 e1 -> case e1 of
      (collectArgs -> (Prim p1, _)) | primName p0 == primName p1 -> return e
      _ -> changed e1

reduceConst _ e = return e
{-# SCC reduceConst #-}

-- | Replace primitives by their "definition" if they would lead to let-bindings
-- with a non-representable type when a function is in ANF. This happens for
-- example when Clash.Size.Vector.map consumes or produces a vector of
-- non-representable elements.
--
-- Basically what this transformation does is replace a primitive the completely
-- unrolled recursive definition that it represents. e.g.
--
-- > zipWith ($) (xs :: Vec 2 (Int -> Int)) (ys :: Vec 2 Int)
--
-- is replaced by:
--
-- > let (x0  :: (Int -> Int))       = case xs  of (:>) _ x xr -> x
-- >     (xr0 :: Vec 1 (Int -> Int)) = case xs  of (:>) _ x xr -> xr
-- >     (x1  :: (Int -> Int)(       = case xr0 of (:>) _ x xr -> x
-- >     (y0  :: Int)                = case ys  of (:>) _ y yr -> y
-- >     (yr0 :: Vec 1 Int)          = case ys  of (:>) _ y yr -> xr
-- >     (y1  :: Int                 = case yr0 of (:>) _ y yr -> y
-- > in  (($) x0 y0 :> ($) x1 y1 :> Nil)
--
-- Currently, it only handles the following functions:
--
-- * Clash.Sized.Vector.zipWith
-- * Clash.Sized.Vector.map
-- * Clash.Sized.Vector.traverse#
-- * Clash.Sized.Vector.fold
-- * Clash.Sized.Vector.foldr
-- * Clash.Sized.Vector.dfold
-- * Clash.Sized.Vector.(++)
-- * Clash.Sized.Vector.head
-- * Clash.Sized.Vector.tail
-- * Clash.Sized.Vector.last
-- * Clash.Sized.Vector.init
-- * Clash.Sized.Vector.unconcat
-- * Clash.Sized.Vector.transpose
-- * Clash.Sized.Vector.replicate
-- * Clash.Sized.Vector.replace_int
-- * Clash.Sized.Vector.imap
-- * Clash.Sized.Vector.dtfold
-- * Clash.Sized.RTree.tdfold
-- * Clash.Sized.RTree.treplicate
-- * Clash.Sized.Internal.BitVector.split#
-- * Clash.Sized.Internal.BitVector.eq#
--
-- Note [Unroll shouldSplit types]
-- 1. Certain higher-order functions over Vec, such as map, have specialized
-- code-paths to turn them into generate-for loops in HDL, instead of having to
-- having to unroll/inline their recursive definitions, e.g. Clash.Sized.Vector.map
--
-- 2. Clash, in general, translates Haskell product types to VHDL records. This
-- mostly works out fine, there is however one exception: certain synthesis
-- tools, and some HDL simulation tools (like verilator), do not like it when
-- the clock (and certain other global control signals) is contained in a
-- record type; they want them to be separate inputs to the entity/module.
-- And Clash actually does some transformations to try to ensure that values of
-- type Clock do not end up in a VHDL record type.
--
-- The problem is that the transformations in 2. never took into account the
-- specialized code-paths in 1. Making the code-paths in 1. aware of the
-- transformations in 2. is really not worth the effort for such a niche case.
-- It's easier to just unroll the recursive definitions.
--
-- See https://github.com/clash-lang/clash-compiler/issues/1606
reduceNonRepPrim :: HasCallStack => NormRewrite
reduceNonRepPrim c@(TransformContext _ ctx) e@(App _ _) | (Prim p, args, ticks) <- collectArgsTicks e = do
  tcm <- Lens.view tcCache
  ultra <- Lens.view normalizeUltra
  let eTy = inferCoreTypeOf tcm e
  let resTy = snd (splitFunForallTy eTy)
  case tyView resTy of
    (TyConApp vecTcNm@(nameOcc -> "Clash.Sized.Vector.Vec")
              [runExcept . tyNatSize tcm -> Right 0, aTy]) -> do
      let nilE = fromMaybe (error "reduceNonRepPrim: unable to create Vec DCs") $ do
            vecTc <- UniqMap.lookup vecTcNm tcm
            [nilCon,consCon] <- pure (tyConDataCons vecTc)
            return (mkVec nilCon consCon aTy 0 [])
      changed (mkTicks nilE ticks)
    tv -> let argLen = length args in case primName p of
      "Clash.Sized.Vector.zipWith"
        | (tmArgs,[lhsElTy,rhsElty,resElTy,nTy]) <- Either.partitionEithers args
        , TyConApp vecTcNm _ <- tv
        , let lhsTy = mkTyConApp vecTcNm [nTy,lhsElTy]
        , let rhsTy = mkTyConApp vecTcNm [nTy,rhsElty]
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure (ultra || n < 2)
                                 , shouldReduce ctx
                                 , List.anyM isUntranslatableType_not_poly
                                        [lhsElTy,rhsElty,resElTy]
                                 -- Note [Unroll shouldSplit types]
                                 , pure (any (Maybe.isJust . shouldSplit tcm)
                                             [lhsTy,rhsTy,resTy]) ]
            if shouldReduce1
               then abstractOverMissingArgs ticks tmArgs eTy c
                      (reduceZipWith p n lhsElTy rhsElty resElTy)
               else return e
          _ -> return e
        | argLen >= 4
        -> error ("reduceNonRepPrim: zipWith bad args" <> showPpr e)

      "Clash.Sized.Vector.map"
        | (tmArgs,[argElTy,resElTy,nTy]) <- Either.partitionEithers args
        , TyConApp vecTcNm _ <- tv
        , let argTy = mkTyConApp vecTcNm [nTy,argElTy]
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure (ultra || n < 2 )
                                 , shouldReduce ctx
                                 , List.anyM isUntranslatableType_not_poly
                                        [argElTy,resElTy]
                                 -- Note [Unroll shouldSplit types]
                                 , pure (any (Maybe.isJust . shouldSplit tcm)
                                             [argTy,resTy]) ]
            if shouldReduce1
               then abstractOverMissingArgs ticks tmArgs eTy c
                      (reduceMap p n argElTy resElTy)
               else return e
          _ -> return e
        | argLen >= 3
        -> error ("reduceNonRepPrim: map bad args" <> showPpr e)

      "Clash.Sized.Vector.traverse#"
        | (tmArgs,[aTy,fTy,bTy,nTy]) <- Either.partitionEithers args
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> abstractOverMissingArgs ticks tmArgs eTy c (reduceTraverse n aTy fTy bTy)
          _ -> return e
        | argLen >= 4
        -> error ("reduceNonRepPrim: traverse# bad args" <> showPpr e)

      "Clash.Sized.Vector.fold"
        | (tmArgs,[nTy,aTy]) <- Either.partitionEithers args
        , (_:Right argTy:_) <- fst (splitFunForallTy (piResultTys tcm (primType p) [nTy,aTy]))
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure (ultra || n == 0)
                                 , shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy))]
            if shouldReduce1 then
              abstractOverMissingArgs ticks tmArgs eTy c (reduceFold (n + 1) aTy)
            else return e
          _ -> return e
        | argLen >= 2
        -> error ("reduceNonRepPrim: fold bad args" <> showPpr e)

      "Clash.Sized.Vector.foldr"
        | (tmArgs,[aTy,bTy,nTy]) <- Either.partitionEithers args
        , (_:_:Right argTy:_) <- fst (splitFunForallTy (piResultTys tcm (primType p) [aTy,bTy,nTy]))
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure ultra
                                 , shouldReduce ctx
                                 , List.anyM isUntranslatableType_not_poly [aTy,bTy]
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy)) ]
            if shouldReduce1
              then abstractOverMissingArgs ticks tmArgs eTy c (reduceFoldr p n aTy)
              else return e
          _ -> return e
        | argLen >= 3
        -> error ("reduceNonRepPrim: foldr bad args" <> showPpr e)

      "Clash.Sized.Vector.dfold"
        | (tmArgs,[_mTy,nTy,aTy]) <- Either.partitionEithers args
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> abstractOverMissingArgs ticks tmArgs eTy c (reduceDFold n aTy)
          _ -> return e
        | argLen >= 3
        -> error ("reduceNonRepPrim: dfold bad args" <> showPpr e)

      "Clash.Sized.Vector.++"
        | (tmArgs,[nTy,aTy,mTy]) <- Either.partitionEithers args
        -> case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy)) of
              (Right n, Right m) -> do
                    shouldReduce1 <- List.orM [ pure (n==0)
                                         , pure (m==0)
                                         , shouldReduce ctx
                                         , isUntranslatableType_not_poly aTy
                                         -- Note [Unroll shouldSplit types]
                                         , pure (Maybe.isJust (shouldSplit tcm resTy)) ]
                    if shouldReduce1
                       then abstractOverMissingArgs ticks tmArgs eTy c (reduceAppend n m aTy)
                       else return e
              _ -> return e
        | argLen >= 3
        -> error ("reduceNonRepPrim: ++ bad args" <> showPpr e)

      "Clash.Sized.Vector.head"
        | (tmArgs,[nTy,aTy]) <- Either.partitionEithers args
        , (Right argTy:_) <- fst (splitFunForallTy (piResultTys tcm (primType p) [nTy,aTy]))
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy)) ]
            if shouldReduce1
               then abstractOverMissingArgs ticks tmArgs eTy c (reduceHead (n+1) aTy)
               else return e
          _ -> return e
        | argLen >= 2
        -> error ("reduceNonRepPrim: head bad args" <> showPpr e)

      "Clash.Sized.Vector.tail"
        | (tmArgs,[nTy,aTy]) <- Either.partitionEithers args
        , (Right argTy:_) <- fst (splitFunForallTy (piResultTys tcm (primType p) [nTy,aTy]))
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy)) ]
            if shouldReduce1
               then abstractOverMissingArgs ticks tmArgs eTy c (reduceTail (n+1) aTy)
               else return e
          _ -> return e
        | argLen >= 2
        -> error ("reduceNonRepPrim: tail bad args" <> showPpr e)

      "Clash.Sized.Vector.last"
        | (tmArgs,[nTy,aTy]) <- Either.partitionEithers args
        , (Right argTy:_) <- fst (splitFunForallTy (piResultTys tcm (primType p) [nTy,aTy]))
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy))
                                 ]
            if shouldReduce1
               then abstractOverMissingArgs ticks tmArgs eTy c (reduceLast (n+1) aTy)
               else return e
          _ -> return e
        | argLen >= 2
        -> error ("reduceNonRepPrim: last bad args" <> showPpr e)

      "Clash.Sized.Vector.init"
        | (tmArgs,[nTy,aTy]) <- Either.partitionEithers args
        , (Right argTy:_) <- fst (splitFunForallTy (piResultTys tcm (primType p) [nTy,aTy]))
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy)) ]
            if shouldReduce1
               then abstractOverMissingArgs ticks tmArgs eTy c (reduceInit p n aTy)
               else return e
          _ -> return e
        | argLen >= 2
        -> error ("reduceNonRepPrim: init bad args" <> showPpr e)

      "Clash.Sized.Vector.unconcat"
        | (tmArgs,[nTy,mTy,aTy]) <- Either.partitionEithers args
        , (_:_:Right argTy:_) <- fst (splitFunForallTy (piResultTys tcm (primType p) [nTy,mTy,aTy]))
        -> case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy)) of
          (Right n, Right m) -> do
            shouldReduce1 <- List.orM [ pure (m==0)
                                      , shouldReduce ctx
                                      , isUntranslatableType_not_poly aTy
                                      --  Note [Unroll shouldSplit types]
                                      , pure (Maybe.isJust (shouldSplit tcm argTy))
                                      ]
            if shouldReduce1 then
              abstractOverMissingArgs ticks tmArgs eTy c (reduceUnconcat p n m aTy)
            else
              return e
          _ -> return e
        | argLen >= 3
        -> error ("reduceNonRepPrim: unconcat bad args" <> showPpr e)

      "Clash.Sized.Vector.transpose"
        | (tmArgs,[mTy,nTy,aTy]) <- Either.partitionEithers args
        -> case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy)) of
          (Right n, Right 0) -> abstractOverMissingArgs ticks tmArgs eTy c (reduceTranspose n 0 aTy)
          _ -> return e
        | argLen >= 3
        -> error ("reduceNonRepPrim: transpose bad args" <> showPpr e)

      "Clash.Sized.Vector.replicate"
        | (tmArgs,[nTy,aTy]) <- Either.partitionEithers args
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm resTy))
                                 ]
            if shouldReduce1
               then abstractOverMissingArgs ticks tmArgs eTy c (reduceReplicate n aTy resTy)
               else return e
          _ -> return e
        | argLen >= 2
        -> error ("reduceNonRepPrim: replicate bad args" <> showPpr e)

       -- replace_int :: KnownNat n => Vec n a -> Int -> a -> Vec n a
      "Clash.Sized.Vector.replace_int"
        | (tmArgs,[nTy,aTy]) <- Either.partitionEithers args
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure ultra
                                 , shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm resTy))
                                 ]
            if shouldReduce1
               then abstractOverMissingArgs ticks tmArgs eTy c (reduceReplace_int n aTy resTy)
               else return e
          _ -> return e
        | argLen >= 2
        -> error ("reduceNonRepPrim: replace_int bad args" <> showPpr e)

      "Clash.Sized.Vector.index_int"
        | (tmArgs,[nTy,aTy]) <- Either.partitionEithers args
        , (_:Right argTy:_) <- fst (splitFunForallTy (piResultTys tcm (primType p) [nTy,aTy]))
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure ultra
                                 , shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy)) ]
            if shouldReduce1
               then abstractOverMissingArgs ticks tmArgs eTy c (reduceIndex_int n aTy)
               else return e
          _ -> return e
        | argLen >= 2
        -> error ("reduceNonRepPrim: index_int bad args" <> showPpr e)

      "Clash.Sized.Vector.imap"
        | (tmArgs,[nTy,argElTy,resElTy]) <- Either.partitionEithers args
        , TyConApp vecTcNm _ <- tv
        , let argTy = mkTyConApp vecTcNm [nTy,argElTy]
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure (ultra || n < 2)
                                 , shouldReduce ctx
                                 , List.anyM isUntranslatableType_not_poly [argElTy,resElTy]
                                 -- Note [Unroll shouldSplit types]
                                 , pure (any (Maybe.isJust . shouldSplit tcm)
                                             [argTy,resTy]) ]
            if shouldReduce1
               then abstractOverMissingArgs ticks tmArgs eTy c (reduceImap n argElTy resElTy)
               else return e
          _ -> return e
        | argLen >= 3
        -> error ("reduceNonRepPrim: imap bad args" <> showPpr e)

      "Clash.Sized.Vector.iterateI"
        | (tmArgs,[nTy,aTy]) <- Either.partitionEithers args
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM
              [ pure (ultra || n < 2)
              , shouldReduce ctx
              , isUntranslatableType_not_poly aTy
              -- Note [Unroll shouldSplit types]
              , pure (Maybe.isJust (shouldSplit tcm resTy)) ]

            if shouldReduce1 then
              abstractOverMissingArgs ticks tmArgs eTy c (reduceIterateI n aTy resTy)
            else
              return e
          _ -> return e
        | argLen >= 2
        -> error ("reduceNonRepPrim: iterateI bad args" <> showPpr e)

      "Clash.Sized.Vector.dtfold"
        | (tmArgs,[_mTy,nTy,aTy]) <- Either.partitionEithers args
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> abstractOverMissingArgs ticks tmArgs eTy c (reduceDTFold n aTy)
          _ -> return e
        | argLen >= 3
        -> error ("reduceNonRepPrim: dtfold bad args" <> showPpr e)

      "Clash.Sized.Vector.reverse"
        | ultra
        , (tmArgs,[nTy,aTy]) <- Either.partitionEithers args
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> abstractOverMissingArgs ticks tmArgs eTy c (reduceReverse n aTy)

      "Clash.Sized.RTree.tdfold"
        | (tmArgs,[_mTy,nTy,aTy]) <- Either.partitionEithers args
        -> case runExcept (tyNatSize tcm nTy) of
          Right n -> abstractOverMissingArgs ticks tmArgs eTy c (reduceTFold n aTy)
          _ -> return e
        | argLen >= 3
        -> error ("reduceNonRepPrim: tdfold bad args" <> showPpr e)
      "Clash.Sized.RTree.treplicate"
        | (tmArgs,[nTy,aTy]) <- Either.partitionEithers args ->
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType False aTy ]
            if shouldReduce1
               then abstractOverMissingArgs ticks tmArgs eTy c (reduceTReplicate n aTy resTy)
               else return e
          _ -> return e
        | argLen >= 2
        -> error ("reduceNonRepPrim: treplicate bad args" <> showPpr e)
      "Clash.Sized.Internal.BitVector.split#"
        | (tmArgs,[nTy,mTy]) <- Either.partitionEithers args ->
        case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy), tv) of
          (Right n, Right m, TyConApp tupTcNm [lTy,rTy])
            | n == 0 -> abstractOverMissingArgs ticks tmArgs eTy c $ \(_kn :: Term) bvArg (_ctx :: TransformContext) -> do
              let tup = mkApps (Data tupDc)
                           [Right lTy
                           ,Right rTy
                           ,Left  bvArg
                           ,Left  (TyApp (Prim removedArg) rTy)
                           ]

              (changed (mkTicks tup ticks) :: NormalizeSession Term)
            | m == 0 -> abstractOverMissingArgs ticks tmArgs eTy c $ \(_kn :: Term) bvArg (_ctx :: TransformContext) -> do
              let tup = mkApps (Data tupDc)
                           [Right lTy
                           ,Right rTy
                           ,Left  (TyApp (Prim removedArg) lTy)
                           ,Left  bvArg
                           ]

              (changed (mkTicks tup ticks) :: NormalizeSession Term)
           where
            tupDc = fromMaybe (error "reduceNonRepPrim: faield to create tup DC") $ do
                    tupTc <- UniqMap.lookup tupTcNm tcm
                    listToMaybe (tyConDataCons tupTc)
          _ -> return e
        | argLen >= 3
        -> error ("reduceNonRepPrim: split# bad args" <> showPpr e)
      "Clash.Sized.Internal.BitVector.eq#"
        | (tmArgs,[nTy]) <- Either.partitionEithers args
        , Right 0 <- runExcept (tyNatSize tcm nTy)
        , TyConApp boolTcNm [] <- tv
        -> abstractOverMissingArgs ticks tmArgs eTy c $ \(_kn :: Term) (_l :: Term) (_r :: Term) (_ctx :: TransformContext) -> do
           let trueDc = fromMaybe (error "reduceNonRepPrim: failed to create True DC") $ do
                  boolTc <- UniqMap.lookup boolTcNm tcm
                  [_falseDc,dc] <- pure (tyConDataCons boolTc)
                  return dc
            in (changed (Data trueDc) :: NormalizeSession Term)
      _ -> return e
  where
    isUntranslatableType_not_poly t = do
      u <- isUntranslatableType False t
      if u
         then return (null $ Lens.toListOf typeFreeVars t)
         else return False

reduceNonRepPrim _ e = return e
{-# SCC reduceNonRepPrim #-}

class AbstractOverMissingArgs a where
  -- | Abstract over a primitive until it is saturated
  abstractOverMissingArgs ::
    HasCallStack =>
    -- | Ticks originally tagged to the applied primitive
    [TickInfo] ->
    -- | Available arguments
    [Term] ->
    -- | The type of the expression containing the applied primitive
    Type ->
    -- | The context in which reduceNonRepPrim was called
    TransformContext ->
    a ->
    NormalizeSession Term

instance AbstractOverMissingArgs (TransformContext -> NormalizeSession Term) where
  abstractOverMissingArgs ticks args _ is f = (`mkTmApps` args) <$> (`mkTicks` ticks) <$> f is

instance AbstractOverMissingArgs a => AbstractOverMissingArgs (Term -> a) where
  abstractOverMissingArgs ticks (t:ts) ty ctx f = abstractOverMissingArgs ticks ts ty ctx (f t)
  abstractOverMissingArgs ticks []     (tyView -> FunTy argTy resTy) (TransformContext is0 ctx) f = do
     newId <- mkInternalVar is0 "arg" argTy
     let ctx1 = TransformContext (extendInScopeSet is0 newId) (LamBody newId : ctx)
     Lam newId <$> abstractOverMissingArgs ticks [] resTy ctx1 (f (Var newId))
  abstractOverMissingArgs _ _ ty _ _ = error ("not a funty: " <> showPpr ty)
