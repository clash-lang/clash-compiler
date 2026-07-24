{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations for compile-time reduction of expressions / primitives.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Transformations.Reduce
  ( reduceBinders
  , reduceConst
  , reduceConstWorker
  , reduceNonRepPrim
  , reduceNonRepPrimWorker
  ) where

import qualified Control.Lens as Lens
import Control.Monad.Trans.Except (runExcept)
import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.Extra as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import GHC.Stack (HasCallStack)

import Clash.Core.FreeVars (typeFreeVars)
import Clash.Core.HasType
import Clash.Core.Name (nameOcc)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Subst (Subst, extendIdSubst, substTm)
import Clash.Core.Term
  ( CoreContext(..), LetBinding, PrimInfo(..), Term(..), TickInfo(..), collectArgs
  , collectArgsTicks, mkApps, mkTicks, mkTmApps)
import Clash.Core.TyCon (TyCon(..), TyConMap, tyConDataCons)
import Clash.Core.Type (Type(..), TypeView(..), mkTyConApp, splitFunForallTy, tyView)
import Clash.Core.Util (mkVec, shouldSplit, tyNatSize, mkInternalVar)
import Clash.Core.VarEnv (extendInScopeSet)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Normalize.PrimitiveReductions
import Clash.Normalize.Primitives (removedArg)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Normalize.Util (shouldReduce)
import Clash.Rewrite.StrategyDSL (TransformSpec, onApp, transform)
import Clash.Rewrite.Types (TransformContext(..), tcCache, normalizeUltra)
import Clash.Rewrite.Util (changed, isUntranslatableType, setChanged, whnfRW)
import qualified Clash.Sized.Internal.BitVector
import qualified Clash.Sized.RTree
import qualified Clash.Sized.Vector
import Clash.Util (textNameLit)

-- | XXX: is given inverse topologically sorted binders, but returns
-- topologically sorted binders
--
-- TODO: check further speed improvements:
--
-- 1. Store the processed binders in a `AEQTrie Expr LetBinding`
--    * Trades O(log n)*acmpTerm `insert` and `lookup` for:
--    * O(e) `insert` and O(e) `lookup`
reduceBinders
  :: Subst
  -> [LetBinding]
  -> [LetBinding]
  -> NormalizeSession (Subst, [LetBinding])
reduceBinders subst0 processed0 =
  go subst0 (Map.fromList [(e, i) | (i, e) <- reverse processed0]) processed0
 where
  -- The index maps a processed binding's RHS to its binder, keyed on
  -- alpha-equivalence ('Ord' on 'Term'). It replaces a linear scan over the
  -- processed bindings that did an alpha-equivalence comparison per entry.
  -- Later insertions shadow earlier ones, like the head-first linear scan
  -- they replace did.
  go !subst _index processed [] = return (subst,processed)
  go !subst index processed ((i,substTm "reduceBinders" subst -> e):rest)
    | (_,_,ticks) <- collectArgsTicks e
    , NoDeDup `notElem` ticks
    , Just i1 <- Map.lookup e index
    = do
      let subst1 = extendIdSubst subst i (Var i1)
      setChanged
      go subst1 index processed rest
    | otherwise
    = go subst (Map.insert e i index) ((i,e):processed) rest
{-# SCC reduceBinders #-}

reduceConst :: TransformSpec
reduceConst = transform "reduceConst" (onApp 'reduceConstWorker)

-- | The 'App' handler of 'reduceConst'.
reduceConstWorker
  :: HasCallStack
  => TransformContext -> Term -> Term -> Term -> NormalizeSession Term
-- An 'App' in an 'AppFun' context is an inner node of an application spine,
-- e.g. the @f a@ inside @f a b c@. Only evaluate at the root (@f a b c@):
-- an under-applied primitive cannot fold, and if @f@ is itself an application
-- (@(g x) a b c@) the evaluator reduces the whole thing to WHNF anyway, so it
-- folds @g x@ as part of folding the root. Skip the evaluator call here.
reduceConstWorker (TransformContext _ (AppFun:_)) e _appFunction _appArgument = return e
reduceConstWorker ctx e _appFunction _appArgument
  | (Prim p0, _) <- collectArgs e
  = whnfRW False ctx e $ \_ctx1 e1 -> case e1 of
      (collectArgs -> (Prim p1, _)) | primName p0 == primName p1 -> return e
      _ -> changed e1

reduceConstWorker _ e _ _ = return e
{-# SCC reduceConstWorker #-}

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
-- Currently, it only handles the functions in 'reduceNonRepPrimImpls'.
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
reduceNonRepPrim :: TransformSpec
reduceNonRepPrim = transform "reduceNonRepPrim" (onApp 'reduceNonRepPrimWorker)

-- | The 'Clash.Core.Term.App' handler of 'reduceNonRepPrim'.
reduceNonRepPrimWorker
  :: HasCallStack
  => TransformContext -> Term -> Term -> Term -> NormalizeSession Term
reduceNonRepPrimWorker c e _appFunction _appArgument
  | (Prim p, args, ticks) <- collectArgsTicks e
  = do
    tcm <- Lens.view tcCache
    case HashMap.lookup (primName p) reduceNonRepPrimImpls of
      Just handler -> do
        ultraArg <- Lens.view normalizeUltra
        let eTy = inferCoreTypeOf tcm e
        let resTy = snd (splitFunForallTy eTy)
        let tv = tyView resTy
        case zeroLengthVecTerm tcm tv of
          Just nilE -> changed (mkTicks nilE ticks)
          Nothing -> handler ReduceNonRepPrimContext
            { transformContext = c
            , originalTerm = e
            , primInfo = p
            , primArguments = args
            , primTicks = ticks
            , tyConMap = tcm
            , ultra = ultraArg
            , termType = eTy
            , resultType = resTy
            , resultTypeView = tv
            }
      Nothing
        -- Any primitive whose result type is @Vec 0 a@ reduces to @Nil@, not
        -- just the ones with a handler. Whether that can be the case here is
        -- decided from the primitive's declared type first, saving the (much
        -- more expensive) type inference of the applied primitive for the
        -- vast majority of primitives.
        | mayReturnVec tcm (primType p)
        , let resTy = snd (splitFunForallTy (inferCoreTypeOf tcm e))
        , Just nilE <- zeroLengthVecTerm tcm (tyView resTy)
        -> changed (mkTicks nilE ticks)
        | otherwise
        -> return e

reduceNonRepPrimWorker _ e _ _ = return e
{-# SCC reduceNonRepPrimWorker #-}

-- | The name of the 'Clash.Sized.Vector.Vec' type constructor.
vecTcName :: Text
vecTcName = $(textNameLit ''Clash.Sized.Vector.Vec)

-- | If the given type view is @Vec 0 a@, return the corresponding @Nil@ term.
zeroLengthVecTerm :: TyConMap -> TypeView -> Maybe Term
zeroLengthVecTerm tcm tv
  | TyConApp vecTcNm [nTy, aTy] <- tv
  , nameOcc vecTcNm == vecTcName
  , Right 0 <- runExcept (tyNatSize tcm nTy)
  = Just $ fromMaybe (error "reduceNonRepPrim: unable to create Vec DCs") $ do
      vecTc <- UniqMap.lookup vecTcNm tcm
      [nilCon,consCon] <- pure (tyConDataCons vecTc)
      return (mkVec nilCon consCon aTy 0 [])
  | otherwise
  = Nothing

-- | Can applying the primitive produce a value whose type has
-- 'Clash.Sized.Vector.Vec' at its head? Decided from the primitive's declared
-- type alone: a result headed by any other concrete type constructor can
-- never instantiate to a @Vec@, while type variables, type family
-- applications, and other opaque heads might. Over-approximating is sound; a
-- 'True' merely makes 'reduceNonRepPrimWorker' infer the type of the applied
-- primitive to check for the @Vec 0@ rewrite.
mayReturnVec :: TyConMap -> Type -> Bool
mayReturnVec tcm = go
 where
  go (ForAllTy _ ty) = go ty
  go ty = case tyView ty of
    FunTy _ resTy -> go resTy
    TyConApp tcNm _
      | nameOcc tcNm == vecTcName -> True
      | otherwise -> case UniqMap.lookup tcNm tcm of
          -- Type families might reduce to a Vec
          Just FunTyCon{} -> True
          Just _ -> False
          Nothing -> True
    OtherType otherTy -> case otherTy of
      LitTy _ -> False
      _ -> True

-- | Everything the handlers in 'reduceNonRepPrimImpls' receive from the
-- dispatch site in 'reduceNonRepPrimWorker'.
data ReduceNonRepPrimContext = ReduceNonRepPrimContext
  { transformContext :: TransformContext
  , originalTerm :: Term
    -- ^ The primitive applied to its arguments
  , primInfo :: PrimInfo
  , primArguments :: [Either Term Type]
  , primTicks :: [TickInfo]
  , tyConMap :: TyConMap
  , ultra :: Bool
    -- ^ Whether @-fclash-ultra@ is enabled
  , termType :: Type
    -- ^ The type of 'originalTerm'
  , resultType :: Type
    -- ^ 'termType' stripped of its quantifiers and function arguments
  , resultTypeView :: TypeView
    -- ^ 'tyView' of 'resultType'
  }

-- | A handler for a specific primitive in 'reduceNonRepPrimImpls'.
type ReduceNonRepPrimHandler
  = ReduceNonRepPrimContext -> NormalizeSession Term

-- | The primitives 'reduceNonRepPrimWorker' can reduce, keyed on primitive
-- name. The handlers are the arms of the @case@ expression this map replaced;
-- a handler whose guards do not apply returns 'originalTerm' unchanged, like
-- the fall-through of the @case@ did.
reduceNonRepPrimImpls :: HashMap Text ReduceNonRepPrimHandler
reduceNonRepPrimImpls = HashMap.fromList
  [ ($(textNameLit 'Clash.Sized.Vector.zipWith), reduceZipWithHandler)
  , ($(textNameLit 'Clash.Sized.Vector.map), reduceMapHandler)
  , ($(textNameLit 'Clash.Sized.Vector.traverse#), reduceTraverseHandler)
  , ($(textNameLit 'Clash.Sized.Vector.fold), reduceFoldHandler)
  , ($(textNameLit 'Clash.Sized.Vector.foldr), reduceFoldrHandler)
  , ($(textNameLit 'Clash.Sized.Vector.dfold), reduceDFoldHandler)
  , ($(textNameLit '(Clash.Sized.Vector.++)), reduceAppendHandler)
  , ($(textNameLit 'Clash.Sized.Vector.head), reduceHeadHandler)
  , ($(textNameLit 'Clash.Sized.Vector.tail), reduceTailHandler)
  , ($(textNameLit 'Clash.Sized.Vector.last), reduceLastHandler)
  , ($(textNameLit 'Clash.Sized.Vector.init), reduceInitHandler)
  , ($(textNameLit 'Clash.Sized.Vector.unconcat), reduceUnconcatHandler)
  , ($(textNameLit 'Clash.Sized.Vector.transpose), reduceTransposeHandler)
  , ($(textNameLit 'Clash.Sized.Vector.replicate), reduceReplicateHandler)
  -- replace_int and index_int are not exported from Clash.Sized.Vector, so
  -- their names cannot be quoted
  , ("Clash.Sized.Vector.replace_int", reduceReplaceIntHandler)
  , ("Clash.Sized.Vector.index_int", reduceIndexIntHandler)
  , ($(textNameLit 'Clash.Sized.Vector.imap), reduceImapHandler)
  , ($(textNameLit 'Clash.Sized.Vector.iterateI), reduceIterateIHandler)
  , ($(textNameLit 'Clash.Sized.Vector.dtfold), reduceDTFoldHandler)
  , ($(textNameLit 'Clash.Sized.Vector.reverse), reduceReverseHandler)
  , ($(textNameLit 'Clash.Sized.RTree.tdfold), reduceTDFoldHandler)
  , ($(textNameLit 'Clash.Sized.RTree.treplicate), reduceTReplicateHandler)
  , ($(textNameLit 'Clash.Sized.Internal.BitVector.split#), reduceSplitHandler)
  , ($(textNameLit 'Clash.Sized.Internal.BitVector.eq#), reduceEqHandler)
  ]

reduceZipWithHandler :: ReduceNonRepPrimHandler
reduceZipWithHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[lhsElTy,rhsElty,resElTy,nTy]) <- Either.partitionEithers primArguments
  , TyConApp vecTcNm _ <- resultTypeView
  , let lhsTy = mkTyConApp vecTcNm [nTy,lhsElTy]
  , let rhsTy = mkTyConApp vecTcNm [nTy,rhsElty]
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM [ pure (ultra || n < 2)
                             , shouldReduce (tfContext transformContext)
                             , List.anyM isUntranslatableType_not_poly
                                    [lhsElTy,rhsElty,resElTy]
                             -- Note [Unroll shouldSplit types]
                             , pure (any (Maybe.isJust . shouldSplit tyConMap)
                                         [lhsTy,rhsTy,resultType]) ]
        if shouldReduce1
           then abstractOverMissingArgs primTicks tmArgs termType transformContext
                  (reduceZipWith primInfo n lhsElTy rhsElty resElTy)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 4
  = error ("reduceNonRepPrim: zipWith bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceMapHandler :: ReduceNonRepPrimHandler
reduceMapHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[argElTy,resElTy,nTy]) <- Either.partitionEithers primArguments
  , TyConApp vecTcNm _ <- resultTypeView
  , let argTy = mkTyConApp vecTcNm [nTy,argElTy]
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM [ pure (ultra || n < 2 )
                             , shouldReduce (tfContext transformContext)
                             , List.anyM isUntranslatableType_not_poly
                                    [argElTy,resElTy]
                             -- Note [Unroll shouldSplit types]
                             , pure (any (Maybe.isJust . shouldSplit tyConMap)
                                         [argTy,resultType]) ]
        if shouldReduce1
           then abstractOverMissingArgs primTicks tmArgs termType transformContext
                  (reduceMap primInfo n argElTy resElTy)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 3
  = error ("reduceNonRepPrim: map bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceTraverseHandler :: ReduceNonRepPrimHandler
reduceTraverseHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[aTy,fTy,bTy,nTy]) <- Either.partitionEithers primArguments
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> abstractOverMissingArgs primTicks tmArgs termType transformContext
                   (reduceTraverse n aTy fTy bTy)
      _ -> return originalTerm
  | length primArguments >= 4
  = error ("reduceNonRepPrim: traverse# bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceFoldHandler :: ReduceNonRepPrimHandler
reduceFoldHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,aTy]) <- Either.partitionEithers primArguments
  , (_:Right argTy:_) <- fst (splitFunForallTy (piResultTys tyConMap (primType primInfo) [nTy,aTy]))
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM [ pure (ultra || n == 0)
                             , shouldReduce (tfContext transformContext)
                             , isUntranslatableType_not_poly aTy
                             -- Note [Unroll shouldSplit types]
                             , pure (Maybe.isJust (shouldSplit tyConMap argTy))]
        if shouldReduce1 then
          abstractOverMissingArgs primTicks tmArgs termType transformContext
            (reduceFold (n + 1) aTy)
        else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = error ("reduceNonRepPrim: fold bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceFoldrHandler :: ReduceNonRepPrimHandler
reduceFoldrHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[aTy,bTy,nTy]) <- Either.partitionEithers primArguments
  , (_:_:Right argTy:_) <- fst (splitFunForallTy (piResultTys tyConMap (primType primInfo) [aTy,bTy,nTy]))
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM [ pure ultra
                             , shouldReduce (tfContext transformContext)
                             , List.anyM isUntranslatableType_not_poly [aTy,bTy]
                             -- Note [Unroll shouldSplit types]
                             , pure (Maybe.isJust (shouldSplit tyConMap argTy)) ]
        if shouldReduce1
          then abstractOverMissingArgs primTicks tmArgs termType transformContext
                 (reduceFoldr primInfo n aTy)
          else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 3
  = error ("reduceNonRepPrim: foldr bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceDFoldHandler :: ReduceNonRepPrimHandler
reduceDFoldHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[_mTy,nTy,aTy]) <- Either.partitionEithers primArguments
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> abstractOverMissingArgs primTicks tmArgs termType transformContext
                   (reduceDFold n aTy)
      _ -> return originalTerm
  | length primArguments >= 3
  = error ("reduceNonRepPrim: dfold bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceAppendHandler :: ReduceNonRepPrimHandler
reduceAppendHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,aTy,mTy]) <- Either.partitionEithers primArguments
  = case (runExcept (tyNatSize tyConMap nTy), runExcept (tyNatSize tyConMap mTy)) of
      (Right n, Right m) -> do
            shouldReduce1 <- List.orM [ pure (n==0)
                                 , pure (m==0)
                                 , shouldReduce (tfContext transformContext)
                                 , isUntranslatableType_not_poly aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (Maybe.isJust (shouldSplit tyConMap resultType)) ]
            if shouldReduce1
               then abstractOverMissingArgs primTicks tmArgs termType transformContext
                      (reduceAppend n m aTy)
               else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 3
  = error ("reduceNonRepPrim: ++ bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceHeadHandler :: ReduceNonRepPrimHandler
reduceHeadHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,aTy]) <- Either.partitionEithers primArguments
  , (Right argTy:_) <- fst (splitFunForallTy (piResultTys tyConMap (primType primInfo) [nTy,aTy]))
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM [ shouldReduce (tfContext transformContext)
                             , isUntranslatableType_not_poly aTy
                             -- Note [Unroll shouldSplit types]
                             , pure (Maybe.isJust (shouldSplit tyConMap argTy)) ]
        if shouldReduce1
           then abstractOverMissingArgs primTicks tmArgs termType transformContext
                  (reduceHead (n+1) aTy)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = error ("reduceNonRepPrim: head bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceTailHandler :: ReduceNonRepPrimHandler
reduceTailHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,aTy]) <- Either.partitionEithers primArguments
  , (Right argTy:_) <- fst (splitFunForallTy (piResultTys tyConMap (primType primInfo) [nTy,aTy]))
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM [ shouldReduce (tfContext transformContext)
                             , isUntranslatableType_not_poly aTy
                             -- Note [Unroll shouldSplit types]
                             , pure (Maybe.isJust (shouldSplit tyConMap argTy)) ]
        if shouldReduce1
           then abstractOverMissingArgs primTicks tmArgs termType transformContext
                  (reduceTail (n+1) aTy)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = error ("reduceNonRepPrim: tail bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceLastHandler :: ReduceNonRepPrimHandler
reduceLastHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,aTy]) <- Either.partitionEithers primArguments
  , (Right argTy:_) <- fst (splitFunForallTy (piResultTys tyConMap (primType primInfo) [nTy,aTy]))
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM [ shouldReduce (tfContext transformContext)
                             , isUntranslatableType_not_poly aTy
                             -- Note [Unroll shouldSplit types]
                             , pure (Maybe.isJust (shouldSplit tyConMap argTy))
                             ]
        if shouldReduce1
           then abstractOverMissingArgs primTicks tmArgs termType transformContext
                  (reduceLast (n+1) aTy)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = error ("reduceNonRepPrim: last bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceInitHandler :: ReduceNonRepPrimHandler
reduceInitHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,aTy]) <- Either.partitionEithers primArguments
  , (Right argTy:_) <- fst (splitFunForallTy (piResultTys tyConMap (primType primInfo) [nTy,aTy]))
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM [ shouldReduce (tfContext transformContext)
                             , isUntranslatableType_not_poly aTy
                             -- Note [Unroll shouldSplit types]
                             , pure (Maybe.isJust (shouldSplit tyConMap argTy)) ]
        if shouldReduce1
           then abstractOverMissingArgs primTicks tmArgs termType transformContext
                  (reduceInit primInfo n aTy)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = error ("reduceNonRepPrim: init bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceUnconcatHandler :: ReduceNonRepPrimHandler
reduceUnconcatHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,mTy,aTy]) <- Either.partitionEithers primArguments
  , (_:_:Right argTy:_) <- fst (splitFunForallTy (piResultTys tyConMap (primType primInfo) [nTy,mTy,aTy]))
  = case (runExcept (tyNatSize tyConMap nTy), runExcept (tyNatSize tyConMap mTy)) of
      (Right n, Right m) -> do
        shouldReduce1 <- List.orM [ pure (m==0)
                                  , shouldReduce (tfContext transformContext)
                                  , isUntranslatableType_not_poly aTy
                                  --  Note [Unroll shouldSplit types]
                                  , pure (Maybe.isJust (shouldSplit tyConMap argTy))
                                  ]
        if shouldReduce1 then
          abstractOverMissingArgs primTicks tmArgs termType transformContext
            (reduceUnconcat primInfo n m aTy)
        else
          return originalTerm
      _ -> return originalTerm
  | length primArguments >= 3
  = error ("reduceNonRepPrim: unconcat bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceTransposeHandler :: ReduceNonRepPrimHandler
reduceTransposeHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[mTy,nTy,aTy]) <- Either.partitionEithers primArguments
  = case (runExcept (tyNatSize tyConMap nTy), runExcept (tyNatSize tyConMap mTy)) of
      (Right n, Right 0) -> abstractOverMissingArgs primTicks tmArgs termType transformContext
                              (reduceTranspose n 0 aTy)
      _ -> return originalTerm
  | length primArguments >= 3
  = error ("reduceNonRepPrim: transpose bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceReplicateHandler :: ReduceNonRepPrimHandler
reduceReplicateHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,aTy]) <- Either.partitionEithers primArguments
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM [ shouldReduce (tfContext transformContext)
                             , isUntranslatableType_not_poly aTy
                             -- Note [Unroll shouldSplit types]
                             , pure (Maybe.isJust (shouldSplit tyConMap resultType))
                             ]
        if shouldReduce1
           then abstractOverMissingArgs primTicks tmArgs termType transformContext
                  (reduceReplicate n aTy resultType)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = error ("reduceNonRepPrim: replicate bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

-- replace_int :: KnownNat n => Vec n a -> Int -> a -> Vec n a
reduceReplaceIntHandler :: ReduceNonRepPrimHandler
reduceReplaceIntHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,aTy]) <- Either.partitionEithers primArguments
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM [ pure ultra
                             , shouldReduce (tfContext transformContext)
                             , isUntranslatableType_not_poly aTy
                             -- Note [Unroll shouldSplit types]
                             , pure (Maybe.isJust (shouldSplit tyConMap resultType))
                             ]
        if shouldReduce1
           then abstractOverMissingArgs primTicks tmArgs termType transformContext
                  (reduceReplace_int n aTy resultType)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = error ("reduceNonRepPrim: replace_int bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceIndexIntHandler :: ReduceNonRepPrimHandler
reduceIndexIntHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,aTy]) <- Either.partitionEithers primArguments
  , (_:Right argTy:_) <- fst (splitFunForallTy (piResultTys tyConMap (primType primInfo) [nTy,aTy]))
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM [ pure ultra
                             , shouldReduce (tfContext transformContext)
                             , isUntranslatableType_not_poly aTy
                             -- Note [Unroll shouldSplit types]
                             , pure (Maybe.isJust (shouldSplit tyConMap argTy)) ]
        if shouldReduce1
           then abstractOverMissingArgs primTicks tmArgs termType transformContext
                  (reduceIndex_int n aTy)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = error ("reduceNonRepPrim: index_int bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceImapHandler :: ReduceNonRepPrimHandler
reduceImapHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,argElTy,resElTy]) <- Either.partitionEithers primArguments
  , TyConApp vecTcNm _ <- resultTypeView
  , let argTy = mkTyConApp vecTcNm [nTy,argElTy]
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM [ pure (ultra || n < 2)
                             , shouldReduce (tfContext transformContext)
                             , List.anyM isUntranslatableType_not_poly [argElTy,resElTy]
                             -- Note [Unroll shouldSplit types]
                             , pure (any (Maybe.isJust . shouldSplit tyConMap)
                                         [argTy,resultType]) ]
        if shouldReduce1
           then abstractOverMissingArgs primTicks tmArgs termType transformContext
                  (reduceImap n argElTy resElTy)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 3
  = error ("reduceNonRepPrim: imap bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceIterateIHandler :: ReduceNonRepPrimHandler
reduceIterateIHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,aTy]) <- Either.partitionEithers primArguments
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM
          [ pure (ultra || n < 2)
          , shouldReduce (tfContext transformContext)
          , isUntranslatableType_not_poly aTy
          -- Note [Unroll shouldSplit types]
          , pure (Maybe.isJust (shouldSplit tyConMap resultType)) ]

        if shouldReduce1 then
          abstractOverMissingArgs primTicks tmArgs termType transformContext
            (reduceIterateI n aTy resultType)
        else
          return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = error ("reduceNonRepPrim: iterateI bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceDTFoldHandler :: ReduceNonRepPrimHandler
reduceDTFoldHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[_mTy,nTy,aTy]) <- Either.partitionEithers primArguments
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> abstractOverMissingArgs primTicks tmArgs termType transformContext
                   (reduceDTFold n aTy)
      _ -> return originalTerm
  | length primArguments >= 3
  = error ("reduceNonRepPrim: dtfold bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceReverseHandler :: ReduceNonRepPrimHandler
reduceReverseHandler ReduceNonRepPrimContext{..}
  | ultra
  , (tmArgs,[nTy,aTy]) <- Either.partitionEithers primArguments
  , Right n <- runExcept (tyNatSize tyConMap nTy)
  = abstractOverMissingArgs primTicks tmArgs termType transformContext
      (reduceReverse n aTy)
  | otherwise
  = return originalTerm

reduceTDFoldHandler :: ReduceNonRepPrimHandler
reduceTDFoldHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[_mTy,nTy,aTy]) <- Either.partitionEithers primArguments
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> abstractOverMissingArgs primTicks tmArgs termType transformContext
                   (reduceTFold n aTy)
      _ -> return originalTerm
  | length primArguments >= 3
  = error ("reduceNonRepPrim: tdfold bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceTReplicateHandler :: ReduceNonRepPrimHandler
reduceTReplicateHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,aTy]) <- Either.partitionEithers primArguments
  = case runExcept (tyNatSize tyConMap nTy) of
      Right n -> do
        shouldReduce1 <- List.orM [ shouldReduce (tfContext transformContext)
                             , isUntranslatableType False aTy ]
        if shouldReduce1
           then abstractOverMissingArgs primTicks tmArgs termType transformContext
                  (reduceTReplicate n aTy resultType)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = error ("reduceNonRepPrim: treplicate bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceSplitHandler :: ReduceNonRepPrimHandler
reduceSplitHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy,mTy]) <- Either.partitionEithers primArguments
  = case (runExcept (tyNatSize tyConMap nTy), runExcept (tyNatSize tyConMap mTy), resultTypeView) of
      (Right n, Right m, TyConApp tupTcNm [lTy,rTy])
        | n == 0 -> abstractOverMissingArgs primTicks tmArgs termType transformContext $
            \(_kn :: Term) bvArg (_ctx :: TransformContext) -> do
              let tup = mkApps (Data tupDc)
                           [Right lTy
                           ,Right rTy
                           ,Left  bvArg
                           ,Left  (TyApp (Prim removedArg) rTy)
                           ]

              (changed (mkTicks tup primTicks) :: NormalizeSession Term)
        | m == 0 -> abstractOverMissingArgs primTicks tmArgs termType transformContext $
            \(_kn :: Term) bvArg (_ctx :: TransformContext) -> do
              let tup = mkApps (Data tupDc)
                           [Right lTy
                           ,Right rTy
                           ,Left  (TyApp (Prim removedArg) lTy)
                           ,Left  bvArg
                           ]

              (changed (mkTicks tup primTicks) :: NormalizeSession Term)
       where
        tupDc = fromMaybe (error "reduceNonRepPrim: faield to create tup DC") $ do
                tupTc <- UniqMap.lookup tupTcNm tyConMap
                listToMaybe (tyConDataCons tupTc)
      _ -> return originalTerm
  | length primArguments >= 3
  = error ("reduceNonRepPrim: split# bad args" <> showPpr originalTerm)
  | otherwise
  = return originalTerm

reduceEqHandler :: ReduceNonRepPrimHandler
reduceEqHandler ReduceNonRepPrimContext{..}
  | (tmArgs,[nTy]) <- Either.partitionEithers primArguments
  , Right 0 <- runExcept (tyNatSize tyConMap nTy)
  , TyConApp boolTcNm [] <- resultTypeView
  = abstractOverMissingArgs primTicks tmArgs termType transformContext $
      \(_kn :: Term) (_l :: Term) (_r :: Term) (_ctx :: TransformContext) ->
        let trueDc = fromMaybe (error "reduceNonRepPrim: failed to create True DC") $ do
              boolTc <- UniqMap.lookup boolTcNm tyConMap
              [_falseDc,dc] <- pure (tyConDataCons boolTc)
              return dc
        in (changed (Data trueDc) :: NormalizeSession Term)
  | otherwise
  = return originalTerm

isUntranslatableType_not_poly :: Type -> NormalizeSession Bool
isUntranslatableType_not_poly t = do
  u <- isUntranslatableType False t
  if u
     then return (null $ Lens.toListOf typeFreeVars t)
     else return False

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
