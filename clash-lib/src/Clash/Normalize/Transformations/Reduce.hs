{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021     , QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations for compile-time reduction of expressions / primitives.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Normalize.Transformations.Reduce
  ( partialEval
  , reduceBinders
  , reduceConst
  , reduceNonRepPrim
  ) where

import Control.Concurrent.Supply (splitSupply)
import qualified Control.Lens as Lens
import Control.Monad.Trans.Except (runExcept)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Maybe as Maybe
import GHC.Stack (HasCallStack)
import System.IO.Unsafe (unsafePerformIO)

import Clash.Core.FreeVars (typeFreeVars)
import Clash.Core.HasType
import Clash.Core.Name (nameOcc)
import Clash.Core.PartialEval
import Clash.Core.PartialEval.AsTerm
import Clash.Core.PartialEval.NormalForm
import Clash.Core.Subst (Subst, extendIdSubst, substTm)
import Clash.Core.Term
  ( LetBinding, PrimInfo(..), Term(..), TickInfo(..), collectArgs
  , collectArgsTicks, mkApps, mkTicks)
import Clash.Core.TyCon (tyConDataCons)
import Clash.Core.Type (TypeView(..), mkTyConApp, tyView)
import Clash.Core.Util (mkVec, shouldSplit, tyNatSize)
import Clash.Normalize.PrimitiveReductions
import Clash.Normalize.Primitives (removedArg)
import Clash.Normalize.Types (NormRewrite, NormalizeSession, normalizeUltra, primitives)
import Clash.Normalize.Util (shouldReduce)
import Clash.Rewrite.Types
import Clash.Rewrite.Util (changed, isUntranslatableType, setChanged, whnfRW)
import Clash.Unique (lookupUniqMap)

partialEval :: NormRewrite
partialEval (TransformContext is0 _) e = do
  (heap,addr) <- Lens.use globalHeap
  fun <- Lens.use curFun
  ids <- Lens.use uniqSupply
  bndrs <- Lens.use bindings
  primMap <- Lens.use (extra.primitives)
  tcm <- Lens.view tcCache
  fuel <- Lens.view fuelLimit
  eval <- Lens.view peEvaluator

  let (ids1, ids2) = splitSupply ids
  let genv = mkGlobalEnv bndrs primMap tcm ids1 fuel mempty addr

  uniqSupply Lens..= ids2

  case unsafePerformIO (nf eval primMap genv is0 (fst fun) e) of
    (!e', !genv') -> do
      let tmHeap = fmap asTerm (genvHeap genv')

      -- If partial eval changes a heap value, prefer the new value
      globalHeap Lens..= (tmHeap <> heap, genvAddr genv')
      changed e'

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
reduceNonRepPrim c@(TransformContext is0 ctx) e@(App _ _) | (Prim p, args, ticks) <- collectArgsTicks e = do
  tcm <- Lens.view tcCache
  ultra <- Lens.use (extra.normalizeUltra)
  let eTy = inferCoreTypeOf tcm e
  case tyView eTy of
    (TyConApp vecTcNm@(nameOcc -> "Clash.Sized.Vector.Vec")
              [runExcept . tyNatSize tcm -> Right 0, aTy]) -> do
      let (Just vecTc) = lookupUniqMap vecTcNm tcm
          [nilCon,consCon] = tyConDataCons vecTc
          nilE = mkVec nilCon consCon aTy 0 []
      changed (mkTicks nilE ticks)
    tv -> let argLen = length args in case primName p of
      "Clash.Sized.Vector.zipWith" | argLen == 7 -> do
        let [lhsElTy,rhsElty,resElTy,nTy] = Either.rights args
            TyConApp vecTcNm _ = tv
            lhsTy = mkTyConApp vecTcNm [nTy,lhsElTy]
            rhsTy = mkTyConApp vecTcNm [nTy,rhsElty]
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure (ultra || n < 2)
                                 , shouldReduce ctx
                                 , List.anyM isUntranslatableType_not_poly
                                        [lhsElTy,rhsElty,resElTy]
                                 -- Note [Unroll shouldSplit types]
                                 , pure (any (Maybe.isJust . shouldSplit tcm)
                                             [lhsTy,rhsTy,eTy]) ]
            if shouldReduce1
               then let [fun,lhsArg,rhsArg] = Either.lefts args
                    in  (`mkTicks` ticks) <$>
                        reduceZipWith c p n lhsElTy rhsElty resElTy fun lhsArg rhsArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.map" | argLen == 5 -> do
        let [argElTy,resElTy,nTy] = Either.rights args
            TyConApp vecTcNm _ = tv
            argTy = mkTyConApp vecTcNm [nTy,argElTy]
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure (ultra || n < 2 )
                                 , shouldReduce ctx
                                 , List.anyM isUntranslatableType_not_poly
                                        [argElTy,resElTy]
                                 -- Note [Unroll shouldSplit types]
                                 , pure (any (Maybe.isJust . shouldSplit tcm)
                                             [argTy,eTy]) ]
            if shouldReduce1
               then let [fun,arg] = Either.lefts args
                    in  (`mkTicks` ticks) <$> reduceMap c p n argElTy resElTy fun arg
               else return e
          _ -> return e
      "Clash.Sized.Vector.traverse#" | argLen == 7 ->
        let [aTy,fTy,bTy,nTy] = Either.rights args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n ->
            let [dict,fun,arg] = Either.lefts args
            in  (`mkTicks` ticks) <$> reduceTraverse c n aTy fTy bTy dict fun arg
          _ -> return e
      "Clash.Sized.Vector.fold" | argLen == 4 -> do
        let ([fun,arg],[nTy,aTy]) = Either.partitionEithers args
            argTy = inferCoreTypeOf tcm arg
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure (ultra || n == 0)
                                 , shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy))]
            if shouldReduce1 then
              (`mkTicks` ticks) <$> reduceFold c (n + 1) aTy fun arg
            else return e
          _ -> return e
      "Clash.Sized.Vector.foldr" | argLen == 6 ->
        let ([fun,start,arg],[aTy,bTy,nTy]) = Either.partitionEithers args
            argTy = inferCoreTypeOf tcm arg
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure ultra
                                 , shouldReduce ctx
                                 , List.anyM isUntranslatableType_not_poly [aTy,bTy]
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy)) ]
            if shouldReduce1
              then (`mkTicks` ticks) <$> reduceFoldr c p n aTy fun start arg
              else return e
          _ -> return e
      "Clash.Sized.Vector.dfold" | argLen == 8 ->
        let ([_kn,_motive,fun,start,arg],[_mTy,nTy,aTy]) = Either.partitionEithers args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> (`mkTicks` ticks) <$> reduceDFold is0 n aTy fun start arg
          _ -> return e
      "Clash.Sized.Vector.++" | argLen == 5 ->
        let [nTy,aTy,mTy] = Either.rights args
            [lArg,rArg]   = Either.lefts args
        in case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy)) of
              (Right n, Right m)
                | n == 0 -> changed rArg
                | m == 0 -> changed lArg
                | otherwise -> do
                    shouldReduce1 <- List.orM [ shouldReduce ctx
                                         , isUntranslatableType_not_poly aTy
                                         -- Note [Unroll shouldSplit types]
                                         , pure (Maybe.isJust (shouldSplit tcm eTy)) ]
                    if shouldReduce1
                       then (`mkTicks` ticks) <$> reduceAppend is0 n m aTy lArg rArg
                       else return e
              _ -> return e
      "Clash.Sized.Vector.head" | argLen == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
            argTy     = inferCoreTypeOf tcm vArg
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy)) ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceHead is0 (n+1) aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.tail" | argLen == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
            argTy     = inferCoreTypeOf tcm vArg
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy)) ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceTail is0 (n+1) aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.last" | argLen == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
            argTy     = inferCoreTypeOf tcm vArg
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy))
                                 ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceLast is0 (n+1) aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.init" | argLen == 3 -> do
        let [nTy,aTy] = Either.rights args
            [vArg]    = Either.lefts args
            argTy     = inferCoreTypeOf tcm vArg
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy)) ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceInit is0 p n aTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Vector.unconcat" | argLen == 6 -> do
        let ([_knN,sm,arg],[nTy,mTy,aTy]) = Either.partitionEithers args
            argTy = inferCoreTypeOf tcm arg
        case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy)) of
          (Right n, Right m) -> do
            shouldReduce1 <- List.orM [ pure (m==0)
                                      , shouldReduce ctx
                                      , isUntranslatableType_not_poly aTy
                                      --  Note [Unroll shouldSplit types]
                                      , pure (Maybe.isJust (shouldSplit tcm argTy))
                                      ]
            if shouldReduce1 then
              (`mkTicks` ticks) <$> reduceUnconcat is0 p n m aTy sm arg
            else
              return e
          _ -> return e
      "Clash.Sized.Vector.transpose" | argLen == 5 -> do
        let ([_knN,arg],[mTy,nTy,aTy]) = Either.partitionEithers args
        case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy)) of
          (Right n, Right 0) -> (`mkTicks` ticks) <$> reduceTranspose n 0 aTy arg
          _ -> return e
      "Clash.Sized.Vector.replicate" | argLen == 4 -> do
        let ([_sArg,vArg],[nTy,aTy]) = Either.partitionEithers args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm eTy))
                                 ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceReplicate n aTy eTy vArg
               else return e
          _ -> return e
       -- replace_int :: KnownNat n => Vec n a -> Int -> a -> Vec n a
      "Clash.Sized.Vector.replace_int" | argLen == 6 -> do
        let ([_knArg,vArg,iArg,aArg],[nTy,aTy]) = Either.partitionEithers args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure ultra
                                 , shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm eTy))
                                 ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceReplace_int is0 n aTy eTy vArg iArg aArg
               else return e
          _ -> return e

      "Clash.Sized.Vector.index_int" | argLen == 5 -> do
        let ([_knArg,vArg,iArg],[nTy,aTy]) = Either.partitionEithers args
            argTy = inferCoreTypeOf tcm vArg
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure ultra
                                 , shouldReduce ctx
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tcm argTy)) ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceIndex_int is0 n aTy vArg iArg
               else return e
          _ -> return e

      "Clash.Sized.Vector.imap" | argLen == 6 -> do
        let [nTy,argElTy,resElTy] = Either.rights args
            TyConApp vecTcNm _ = tv
            argTy = mkTyConApp vecTcNm [nTy,argElTy]
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ pure (ultra || n < 2)
                                 , shouldReduce ctx
                                 , List.anyM isUntranslatableType_not_poly [argElTy,resElTy]
                                 -- Note [Unroll shouldSplit types]
                                 , pure (any (Maybe.isJust . shouldSplit tcm)
                                             [argTy,eTy]) ]
            if shouldReduce1
               then let [_,fun,arg] = Either.lefts args
                    in  (`mkTicks` ticks) <$> reduceImap c n argElTy resElTy fun arg
               else return e
          _ -> return e
      "Clash.Sized.Vector.iterateI" | argLen == 5 ->
        let ([_kn,f,a],[nTy,aTy]) = Either.partitionEithers args in
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM
              [ pure (ultra || n < 2)
              , shouldReduce ctx
              , isUntranslatableType_not_poly aTy
              -- Note [Unroll shouldSplit types]
              , pure (Maybe.isJust (shouldSplit tcm eTy)) ]

            if shouldReduce1 then
              (`mkTicks` ticks) <$> reduceIterateI c n aTy eTy f a
            else
              return e
          _ -> return e
      "Clash.Sized.Vector.dtfold" | argLen == 8 ->
        let ([_kn,_motive,lrFun,brFun,arg],[_mTy,nTy,aTy]) = Either.partitionEithers args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> (`mkTicks` ticks) <$> reduceDTFold is0 n aTy lrFun brFun arg
          _ -> return e

      "Clash.Sized.Vector.reverse"
        | ultra
        , ([vArg],[nTy,aTy]) <- Either.partitionEithers args
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> (`mkTicks` ticks) <$> reduceReverse is0 n aTy vArg

      "Clash.Sized.RTree.tdfold" | argLen == 8 ->
        let ([_kn,_motive,lrFun,brFun,arg],[_mTy,nTy,aTy]) = Either.partitionEithers args
        in  case runExcept (tyNatSize tcm nTy) of
          Right n -> (`mkTicks` ticks) <$> reduceTFold is0 n aTy lrFun brFun arg
          _ -> return e
      "Clash.Sized.RTree.treplicate" | argLen == 4 -> do
        let ([_sArg,vArg],[nTy,aTy]) = Either.partitionEithers args
        case runExcept (tyNatSize tcm nTy) of
          Right n -> do
            shouldReduce1 <- List.orM [ shouldReduce ctx
                                 , isUntranslatableType False aTy ]
            if shouldReduce1
               then (`mkTicks` ticks) <$> reduceTReplicate n aTy eTy vArg
               else return e
          _ -> return e
      "Clash.Sized.Internal.BitVector.split#" | argLen == 4 -> do
        let ([_knArg,bvArg],[nTy,mTy]) = Either.partitionEithers args
        case (runExcept (tyNatSize tcm nTy), runExcept (tyNatSize tcm mTy), tv) of
          (Right n, Right m, TyConApp tupTcNm [lTy,rTy])
            | n == 0 -> do
              let (Just tupTc) = lookupUniqMap tupTcNm tcm
                  [tupDc]      = tyConDataCons tupTc
                  tup          = mkApps (Data tupDc)
                                    [Right lTy
                                    ,Right rTy
                                    ,Left  bvArg
                                    ,Left  (TyApp (Prim removedArg) rTy)
                                    ]

              changed (mkTicks tup ticks)
            | m == 0 -> do
              let (Just tupTc) = lookupUniqMap tupTcNm tcm
                  [tupDc]      = tyConDataCons tupTc
                  tup          = mkApps (Data tupDc)
                                    [Right lTy
                                    ,Right rTy
                                    ,Left  (TyApp (Prim removedArg) lTy)
                                    ,Left  bvArg
                                    ]

              changed (mkTicks tup ticks)
          _ -> return e
      "Clash.Sized.Internal.BitVector.eq#"
        | ([_,_],[nTy]) <- Either.partitionEithers args
        , Right 0 <- runExcept (tyNatSize tcm nTy)
        , TyConApp boolTcNm [] <- tv
        -> let (Just boolTc) = lookupUniqMap boolTcNm tcm
               [_falseDc,trueDc] = tyConDataCons boolTc
           in  changed (mkTicks (Data trueDc) ticks)
      _ -> return e
  where
    isUntranslatableType_not_poly t = do
      u <- isUntranslatableType False t
      if u
         then return (null $ Lens.toListOf typeFreeVars t)
         else return False

reduceNonRepPrim _ e = return e
{-# SCC reduceNonRepPrim #-}
