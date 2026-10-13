{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations for compile-time reduction of expressions / primitives, on
  scoped Clash Core.

  = Primitive reductions

  'reducePrim' decides, per primitive, whether to replace an application of
  it by its unrolled definition. The definitions are those of
  "Clash.Normalize.Scoped.PrimitiveReductions". The handlers describe the
  reduction they want as a 'PrimReduction', 'reduceWith' abstracts over
  missing arguments, and 'runPrimReduction' runs the reduction on the
  saturated application.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Scoped.Transformations.Reduce
  ( reduceBinders
  , reduceConst
  , reduceConstWorker
  , reducePrim
  , reducePrimWorker
    -- * Primitive reductions
  , PrimReduction (..)
  , runPrimReduction
  ) where

import qualified Control.Lens as Lens
import Control.Monad.Trans.Except (runExcept)
import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.Extra as List
import qualified Data.Maybe as Maybe
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import GHC.Stack (HasCallStack)

import Clash.Core.Basic (WorkInfo (..))
import qualified Clash.Core.Name as C
import Clash.Core.Name (nameOcc)
import Clash.Core.Pretty (showPpr)
import Clash.Core.TyCon (TyCon (..), tyConDataCons)
import qualified Clash.Normalize.Scoped.PrimitiveReductions as PR
import Clash.Normalize.Scoped.Primitives (removedArg)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.StrategyDSL (Transformation, onApp, toScopedTransformation)
import Clash.Rewrite.Types (normalizeUltra, scopedTypeEnv)
import qualified Clash.Sized.Internal.BitVector
import qualified Clash.Sized.RTree
import qualified Clash.Sized.Vector
import Clash.Util (textNameLit)

import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.FreeVars (hasFreeVarsType)
import Clash.Core.Scoped.HasType (inferCoreTypeOf, piResultTys, primInfoType)
import Clash.Core.Scoped.NameMap (NameMap, insertNameMap)
import Clash.Core.Scoped.Pretty ()
import Clash.Core.Scoped.Subst (unsinkType, withFreshIdBinder)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (collectArgs, collectArgsTicks, mkApps, mkTicks, mkTmApps)
import Clash.Core.Scoped.Type
  ( PiTele (..), SplitFunForall (..), TypeView (..), coreView, mkTyConApp
  , piTeleNameBinders, splitFunForallTy, tyNatSize, tyView )
import Clash.Core.Scoped.TypeEnv (TypeEnv, lookupTyCon)
import Clash.Core.Scoped.Util (mkVec, shouldSplit)
import Clash.Normalize.Scoped.Util (shouldReduce)
import Clash.Rewrite.Scoped
  ( FrameKind (..), TransformContext, Under (LamBody)
  , changed, ctxFrames, pushUnder, setChanged, tfScope, whnfRW )
import Clash.Rewrite.Scoped.Util
  (LetBinding (..), isUntranslatableType, letBindingVar, substVars)

-- | Remove the bindings whose right-hand side is alpha-equivalent to that of
-- a binding before it, substituting the variable of the earlier binding for
-- theirs.
--
-- Is given the bindings of a let-expression in inverse topological order,
-- e.g. opened with 'Clash.Rewrite.Scoped.Util.openLetBindings', and returns
-- the remaining ones in topological order, with the substitution applied to
-- their right-hand sides, together with the substitution, which the caller
-- applies to the body (and to the right-hand sides, if the bindings were not
-- in inverse topological order because they are recursive). Doesn't touch
-- bindings marked 'NoDeDup'.
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
  :: Distinct l
  => Scope l
  -> NameMap TmName l (Term l)
  -- ^ The substitution so far
  -> [LetBinding l]
  -- ^ The processed bindings, in reverse order
  -> [LetBinding l]
  -- ^ The bindings to process
  -> NormalizeSession (NameMap TmName l (Term l), [LetBinding l])
reduceBinders _ !subst processed [] = return (subst, processed)
reduceBinders sc !subst processed (lb : rest)
  | (_, _, ticks) <- collectArgsTicks e
  , not (any isNoDeDup ticks)
  , Just lb1 <- List.find (aeqTm e . lbRhs) processed
  = do
    setChanged
    reduceBinders sc (insertNameMap (lbName lb) (letBindingVar lb1) subst) processed rest
  | otherwise
  = reduceBinders sc subst (lb {lbRhs = e} : processed) rest
 where
  e = substVars sc subst (lbRhs lb)

  isNoDeDup = \case
    NoDeDup -> True
    _ -> False
{-# SCC reduceBinders #-}

-- | Evaluate applied primitives to WHNF with the evaluator
reduceConst :: Transformation
reduceConst = toScopedTransformation "reduceConst" (onApp 'reduceConstWorker)

-- | The 'App' handler of 'reduceConst'
reduceConstWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> Term n -> Term n -> NormalizeSession (Term n)
reduceConstWorker ctx e _appFunction _appArgument = case ctxFrames ctx of
  -- An 'App' in an 'AppFun' context is an inner node of an application spine,
  -- e.g. the @f a@ inside @f a b c@. Only evaluate at the root (@f a b c@):
  -- an under-applied primitive cannot fold, and if @f@ is itself an
  -- application (@(g x) a b c@) the evaluator reduces the whole thing to WHNF
  -- anyway, so it folds @g x@ as part of folding the root. Skip the evaluator
  -- call here.
  KAppFun : _ -> return e
  _ | (Prim p0, _) <- collectArgs e ->
      whnfRW False ctx e $ \_ctx1 e1 -> case collectArgs e1 of
        (Prim p1, _) | primName p0 == primName p1 -> return (sink e)
        _ -> changed e1
    | otherwise -> return e
{-# SCC reduceConstWorker #-}

{- Note [Unroll shouldSplit types]
1. Certain higher-order functions over Vec, such as map, have specialized
code-paths to turn them into generate-for loops in HDL, instead of having to
having to unroll/inline their recursive definitions, e.g. Clash.Sized.Vector.map

2. Clash, in general, translates Haskell product types to VHDL records. This
mostly works out fine, there is however one exception: certain synthesis
tools, and some HDL simulation tools (like verilator), do not like it when
the clock (and certain other global control signals) is contained in a
record type; they want them to be separate inputs to the entity/module.
And Clash actually does some transformations to try to ensure that values of
type Clock do not end up in a VHDL record type.

The problem is that the transformations in 2. never took into account the
specialized code-paths in 1. Making the code-paths in 1. aware of the
transformations in 2. is really not worth the effort for such a niche case.
It's easier to just unroll the recursive definitions.

See https://github.com/clash-lang/clash-compiler/issues/1606
-}

-- | Replace primitives by their "definition" if they would lead to
-- let-bindings with a non-representable type when a function is in ANF. This
-- happens for example when Clash.Size.Vector.map consumes or produces a vector
-- of non-representable elements.
--
-- This transformation used to be called @reduceNonRepPrim@, but nowadays it
-- also replaces primitives for other reasons: e.g., because they are applied
-- to short vectors, because they occur in a context that should be reduced
-- (see 'shouldReduce'), because their argument or result type should be split
-- (see Note [Unroll shouldSplit types]), or because @-fclash-ultra@ is
-- enabled. The conditions differ per primitive, see the handlers in
-- 'reducePrimImpls'.
--
-- Basically what this transformation does is replace a primitive the
-- completely unrolled recursive definition that it represents. e.g.
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
-- Currently, it only handles the functions in 'reducePrimImpls'. See the
-- module header for how the handlers work.
reducePrim :: Transformation
reducePrim = toScopedTransformation "reducePrim" (onApp 'reducePrimWorker)

-- | The 'App' handler of 'reducePrim'
reducePrimWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n -> Term n -> Term n -> Term n -> NormalizeSession (Term n)
reducePrimWorker ctx e _appFunction _appArgument = case ctxFrames ctx of
  -- Only consider the root of an application spine (see 'reduceConst'): the
  -- root sees all arguments, and the @Vec 0@-to-@Nil@ rewrite below is only
  -- type-correct at the root, where no more arguments follow.
  KAppFun : _ -> return e
  _ | (Prim p, args, ticks) <- collectArgsTicks e -> do
      te <- Lens.view scopedTypeEnv
      let handlerM = HashMap.lookup (primName p) reducePrimImpls
          sc = tfScope ctx
      -- Every primitive whose result type is @Vec 0 a@ reduces to @Nil@, not
      -- just the ones with a handler. That takes the type of the applied
      -- primitive, which is expensive to infer, so for a primitive without a
      -- handler it is only inferred when the primitive's declared type shows a
      -- @Vec@ result is possible at all. That rules out the vast majority of
      -- them.
      if Maybe.isNothing handlerM && not (mayReturnVec te (primType p))
      then return e
      else do
        let eTy = inferCoreTypeOf te sc e
        case splitFunForallTy eTy of
          SplitFunForall remaining resTyL ->
            -- The result type mentions a type variable that is not
            -- instantiated yet: the primitive lacks type arguments, so it is
            -- not saturated, and none of the handlers apply.
            case unsinkType (piTeleNameBinders remaining) resTyL of
              Nothing -> return e
              Just resTy -> do
                let tv = tyView (coreView te sc resTy)
                case zeroLengthVecTerm te sc tv of
                  -- Only replace the whole application by @Nil@ if the
                  -- primitive is fully applied (a partially applied primitive
                  -- has a function type, so replacing it by @Nil@ would change
                  -- its arity) and if it does not always perform work (e.g.
                  -- blackboxes like an VIO must be rendered even if their
                  -- result is zero-width).
                  Just nilE
                    | PiNil <- remaining
                    , primWorkInfo p /= WorkAlways
                    -> changed (mkTicks nilE ticks)
                  _ -> case handlerM of
                    Nothing -> return e
                    Just handler -> do
                      ultraArg <- Lens.view normalizeUltra
                      runReducePrimHandler handler ReducePrimContext
                        { transformContext = ctx
                        , originalTerm = e
                        , primInfo = p
                        , primArguments = args
                        , primTicks = ticks
                        , typeEnv = te
                        , ultra = ultraArg
                        , termType = eTy
                        , resultType = resTy
                        , resultTypeView = tv
                        }
    | otherwise -> return e
{-# SCC reducePrimWorker #-}

-- | The name of the 'Clash.Sized.Vector.Vec' type constructor.
vecTcName :: Text
vecTcName = $(textNameLit ''Clash.Sized.Vector.Vec)

-- | If the given type view is @Vec 0 a@, return the corresponding @Nil@ term.
zeroLengthVecTerm :: Distinct n => TypeEnv -> Scope n -> TypeView n -> Maybe (Term n)
zeroLengthVecTerm te sc tv
  | TyConApp vecTcNm [nTy, aTy] <- tv
  , nameOcc vecTcNm == vecTcName
  , Right 0 <- runExcept (tyNatSize te sc nTy)
  = Just $ fromMaybe (error "reducePrim: unable to create Vec DCs") $ do
      vecTc <- lookupTyCon te vecTcNm
      [nilCon, consCon] <- pure (tyConDataCons vecTc)
      return (mkVec te sc nilCon consCon aTy 0 [])
  | otherwise
  = Nothing

-- | Can applying the primitive produce a value whose type has
-- 'Clash.Sized.Vector.Vec' at its head? This is decided from the primitive's
-- declared type ('primType'), which is readily available, rather than from the
-- type of the applied primitive, which has to be inferred: this check is there
-- to avoid work for the vast majority of primitives.
--
-- A result headed by a concrete type constructor other than @Vec@ can never
-- instantiate to a @Vec@. Everything else -- type variables, type families,
-- type constructors we know nothing about -- might, and yields 'True'.
-- Over-approximating is sound: a 'True' only makes 'reducePrim' infer
-- the type of the applied primitive and ask 'zeroLengthVecTerm' for a verdict.
mayReturnVec :: TypeEnv -> Type n -> Bool
mayReturnVec te ty = case splitFunForallTy ty of
  SplitFunForall _ res -> case tyView res of
    TyConApp tcNm _
      | nameOcc tcNm == vecTcName -> True
      | otherwise -> case lookupTyCon te tcNm of
          -- Type families might reduce to a 'Vec'
          Just FunTyCon{} -> True
          Just _ -> False
          Nothing -> True
    _ -> True

-- | Everything the handlers in 'reducePrimImpls' receive from the dispatch
-- site in 'reducePrim'
data ReducePrimContext n = ReducePrimContext
  { transformContext :: TransformContext n
  , originalTerm :: Term n
    -- ^ The primitive applied to its arguments
  , primInfo :: PrimInfo
  , primArguments :: [Either (Term n) (Type n)]
  , primTicks :: [TickInfo n]
  , typeEnv :: TypeEnv
  , ultra :: Bool
    -- ^ Whether @-fclash-ultra@ is enabled
  , termType :: Type n
    -- ^ The type of 'originalTerm'
  , resultType :: Type n
    -- ^ 'termType' stripped of its quantifiers and function arguments
  , resultTypeView :: TypeView n
    -- ^ 'tyView' of the 'coreView' of 'resultType'
  }

-- | The scope of the primitive application
rpcScope :: ReducePrimContext n -> Scope n
rpcScope = tfScope . transformContext

-- | A handler for a specific primitive in 'reducePrimImpls'
newtype ReducePrimHandler = ReducePrimHandler
  { runReducePrimHandler
      :: forall n. Distinct n => ReducePrimContext n -> NormalizeSession (Term n) }

-- | The primitives 'reducePrim' can reduce, keyed on primitive name. A
-- handler whose guards do not apply returns 'originalTerm' unchanged.
reducePrimImpls :: HashMap Text ReducePrimHandler
reducePrimImpls = HashMap.fromList
  [ ($(textNameLit 'Clash.Sized.Vector.zipWith), ReducePrimHandler reduceZipWithHandler)
  , ($(textNameLit 'Clash.Sized.Vector.map), ReducePrimHandler reduceMapHandler)
  , ($(textNameLit 'Clash.Sized.Vector.traverse#), ReducePrimHandler reduceTraverseHandler)
  , ($(textNameLit 'Clash.Sized.Vector.fold), ReducePrimHandler reduceFoldHandler)
  , ($(textNameLit 'Clash.Sized.Vector.foldr), ReducePrimHandler reduceFoldrHandler)
  , ($(textNameLit 'Clash.Sized.Vector.dfold), ReducePrimHandler reduceDFoldHandler)
  , ($(textNameLit '(Clash.Sized.Vector.++)), ReducePrimHandler reduceAppendHandler)
  , ($(textNameLit 'Clash.Sized.Vector.head), ReducePrimHandler reduceHeadHandler)
  , ($(textNameLit 'Clash.Sized.Vector.tail), ReducePrimHandler reduceTailHandler)
  , ($(textNameLit 'Clash.Sized.Vector.last), ReducePrimHandler reduceLastHandler)
  , ($(textNameLit 'Clash.Sized.Vector.init), ReducePrimHandler reduceInitHandler)
  , ($(textNameLit 'Clash.Sized.Vector.unconcat), ReducePrimHandler reduceUnconcatHandler)
  , ($(textNameLit 'Clash.Sized.Vector.transpose), ReducePrimHandler reduceTransposeHandler)
  , ($(textNameLit 'Clash.Sized.Vector.replicate), ReducePrimHandler reduceReplicateHandler)
  -- replace_int and index_int are not exported from Clash.Sized.Vector, so
  -- their names cannot be quoted
  , ("Clash.Sized.Vector.replace_int", ReducePrimHandler reduceReplaceIntHandler)
  , ("Clash.Sized.Vector.index_int", ReducePrimHandler reduceIndexIntHandler)
  , ($(textNameLit 'Clash.Sized.Vector.imap), ReducePrimHandler reduceImapHandler)
  , ($(textNameLit 'Clash.Sized.Vector.iterateI), ReducePrimHandler reduceIterateIHandler)
  , ($(textNameLit 'Clash.Sized.Vector.dtfold), ReducePrimHandler reduceDTFoldHandler)
  , ($(textNameLit 'Clash.Sized.Vector.reverse), ReducePrimHandler reduceReverseHandler)
  , ($(textNameLit 'Clash.Sized.RTree.tdfold), ReducePrimHandler reduceTDFoldHandler)
  , ($(textNameLit 'Clash.Sized.RTree.treplicate), ReducePrimHandler reduceTReplicateHandler)
  , ($(textNameLit 'Clash.Sized.Internal.BitVector.split#), ReducePrimHandler reduceSplitHandler)
  , ($(textNameLit 'Clash.Sized.Internal.BitVector.eq#), ReducePrimHandler reduceEqHandler)
  ]

-- | 'tyNatSize' in the scope of the primitive application
natSize :: Distinct n => ReducePrimContext n -> Type n -> Either String Integer
natSize rpc = runExcept . tyNatSize (typeEnv rpc) (rpcScope rpc)

-- | Whether to unroll a primitive with this argument or result type, see
-- Note [Unroll shouldSplit types]
isSplit :: Distinct n => ReducePrimContext n -> Type n -> Bool
isSplit rpc = Maybe.isJust . shouldSplit (typeEnv rpc) (rpcScope rpc)

-- | The leading argument types of the primitive, instantiated with the given
-- type arguments: the types of its term arguments up to the first remaining
-- quantified type variable.
primArgTys :: Distinct n => ReducePrimContext n -> [Type n] -> [Type n]
primArgTys rpc tys =
  case splitFunForallTy (piResultTys te sc (primInfoType (primInfo rpc)) tys) of
    SplitFunForall tele _ -> leadingArgs tele
 where
  te = typeEnv rpc
  sc = rpcScope rpc

  leadingArgs :: PiTele n l -> [Type n]
  leadingArgs = \case
    PiArg a rest -> a : leadingArgs rest
    _ -> []

-- | 'shouldReduce' in the context of the primitive application
shouldReduceHere :: ReducePrimContext n -> NormalizeSession Bool
shouldReduceHere = shouldReduce . transformContext

-- | The error for a primitive applied to unexpected arguments
badArgs :: Distinct n => String -> ReducePrimContext n -> a
badArgs nm rpc = error ("reducePrim: " <> nm <> " bad args" <> showPpr (originalTerm rpc))

reduceZipWithHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceZipWithHandler rpc@ReducePrimContext{..}
  | (_,[lhsElTy,rhsElty,resElTy,nTy]) <- Either.partitionEithers primArguments
  , TyConApp vecTcNm _ <- resultTypeView
  , let lhsTy = mkTyConApp vecTcNm [nTy,lhsElTy]
  , let rhsTy = mkTyConApp vecTcNm [nTy,rhsElty]
  = case natSize rpc nTy of
      Right n -> do
        lhsNonRep <- isUntranslatableType_not_poly (rpcScope rpc) lhsElTy
        rhsNonRep <- isUntranslatableType_not_poly (rpcScope rpc) rhsElty
        resNonRep <- isUntranslatableType_not_poly (rpcScope rpc) resElTy
        shouldReduce1 <- List.orM [ pure (ultra || n < 2)
                             , shouldReduceHere rpc
                             , pure (lhsNonRep || rhsNonRep || resNonRep)
                             -- Note [Unroll shouldSplit types]
                             , pure (any (isSplit rpc) [lhsTy,rhsTy,resultType]) ]
        if shouldReduce1
           then reduceWith rpc (ReduceZipWith n (not lhsNonRep) (not rhsNonRep))
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 4
  = badArgs "zipWith" rpc
  | otherwise
  = return originalTerm

reduceMapHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceMapHandler rpc@ReducePrimContext{..}
  | (_,[argElTy,resElTy,nTy]) <- Either.partitionEithers primArguments
  , TyConApp vecTcNm _ <- resultTypeView
  , let argTy = mkTyConApp vecTcNm [nTy,argElTy]
  = case natSize rpc nTy of
      Right n -> do
        argNonRep <- isUntranslatableType_not_poly (rpcScope rpc) argElTy
        resNonRep <- isUntranslatableType_not_poly (rpcScope rpc) resElTy
        shouldReduce1 <- List.orM [ pure (ultra || n < 2 )
                             , shouldReduceHere rpc
                             , pure (argNonRep || resNonRep)
                             -- Note [Unroll shouldSplit types]
                             , pure (any (isSplit rpc) [argTy,resultType]) ]
        if shouldReduce1
           then reduceWith rpc (ReduceMap n (not argNonRep))
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 3
  = badArgs "map" rpc
  | otherwise
  = return originalTerm

reduceTraverseHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceTraverseHandler rpc@ReducePrimContext{..}
  | (_,[_aTy,_fTy,_bTy,nTy]) <- Either.partitionEithers primArguments
  = case natSize rpc nTy of
      Right n -> reduceWith rpc (ReduceTraverse n)
      _ -> return originalTerm
  | length primArguments >= 4
  = badArgs "traverse#" rpc
  | otherwise
  = return originalTerm

reduceFoldHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceFoldHandler rpc@ReducePrimContext{..}
  | (_,[nTy,aTy]) <- Either.partitionEithers primArguments
  , (_:argTy:_) <- primArgTys rpc [nTy,aTy]
  = case natSize rpc nTy of
      Right n -> do
        shouldReduce1 <- List.orM [ pure (ultra || n == 0)
                             , shouldReduceHere rpc
                             , isUntranslatableType_not_poly (rpcScope rpc) aTy
                             -- Note [Unroll shouldSplit types]
                             , pure (isSplit rpc argTy)]
        if shouldReduce1
          then reduceWith rpc (ReduceFold (n + 1))
          else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = badArgs "fold" rpc
  | otherwise
  = return originalTerm

reduceFoldrHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceFoldrHandler rpc@ReducePrimContext{..}
  | (_,[aTy,bTy,nTy]) <- Either.partitionEithers primArguments
  , (_:_:argTy:_) <- primArgTys rpc [aTy,bTy,nTy]
  = case natSize rpc nTy of
      Right n -> do
        aNonRep <- isUntranslatableType_not_poly (rpcScope rpc) aTy
        bNonRep <- isUntranslatableType_not_poly (rpcScope rpc) bTy
        shouldReduce1 <- List.orM [ pure ultra
                             , shouldReduceHere rpc
                             , pure (aNonRep || bNonRep)
                             -- Note [Unroll shouldSplit types]
                             , pure (isSplit rpc argTy) ]
        if shouldReduce1
          then reduceWith rpc (ReduceFoldr n (not aNonRep))
          else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 3
  = badArgs "foldr" rpc
  | otherwise
  = return originalTerm

reduceDFoldHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceDFoldHandler rpc@ReducePrimContext{..}
  | (_,[_mTy,nTy,_aTy]) <- Either.partitionEithers primArguments
  = case natSize rpc nTy of
      Right n -> reduceWith rpc (ReduceDFold n)
      _ -> return originalTerm
  | length primArguments >= 3
  = badArgs "dfold" rpc
  | otherwise
  = return originalTerm

reduceAppendHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceAppendHandler rpc@ReducePrimContext{..}
  | (_,[nTy,aTy,mTy]) <- Either.partitionEithers primArguments
  = case (natSize rpc nTy, natSize rpc mTy) of
      (Right n, Right m) -> do
            shouldReduce1 <- List.orM [ pure (n==0)
                                 , pure (m==0)
                                 , shouldReduceHere rpc
                                 , isUntranslatableType_not_poly (rpcScope rpc) aTy
                                 -- Note [Unroll shouldSplit types]
                                 , pure (isSplit rpc resultType) ]
            if shouldReduce1
               then reduceWith rpc (ReduceAppend n m)
               else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 3
  = badArgs "++" rpc
  | otherwise
  = return originalTerm

-- | The handler of @head@, @tail@ and @last@: primitives of type @forall n
-- a. Vec (n + 1) a -> ...@
reduceVecPrimHandler
  :: Distinct n
  => String
  -> (Integer -> PrimReduction)
  -- ^ The reduction, given the length of the vector argument
  -> ReducePrimContext n
  -> NormalizeSession (Term n)
reduceVecPrimHandler nm red rpc@ReducePrimContext{..}
  | (_,[nTy,aTy]) <- Either.partitionEithers primArguments
  , (argTy:_) <- primArgTys rpc [nTy,aTy]
  = case natSize rpc nTy of
      Right n -> do
        shouldReduce1 <- List.orM [ shouldReduceHere rpc
                             , isUntranslatableType_not_poly (rpcScope rpc) aTy
                             -- Note [Unroll shouldSplit types]
                             , pure (isSplit rpc argTy) ]
        if shouldReduce1
           then reduceWith rpc (red (n+1))
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = badArgs nm rpc
  | otherwise
  = return originalTerm

reduceHeadHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceHeadHandler = reduceVecPrimHandler "head" ReduceHead

reduceTailHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceTailHandler = reduceVecPrimHandler "tail" ReduceTail

reduceLastHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceLastHandler = reduceVecPrimHandler "last" ReduceLast

reduceInitHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceInitHandler rpc@ReducePrimContext{..}
  | (_,[nTy,aTy]) <- Either.partitionEithers primArguments
  , (argTy:_) <- primArgTys rpc [nTy,aTy]
  = case natSize rpc nTy of
      Right n -> do
        aNonRep <- isUntranslatableType_not_poly (rpcScope rpc) aTy
        shouldReduce1 <- List.orM [ shouldReduceHere rpc
                             , pure aNonRep
                             -- Note [Unroll shouldSplit types]
                             , pure (isSplit rpc argTy) ]
        if shouldReduce1
           then reduceWith rpc (ReduceInit n (not aNonRep))
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = badArgs "init" rpc
  | otherwise
  = return originalTerm

reduceUnconcatHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceUnconcatHandler rpc@ReducePrimContext{..}
  | (_,[nTy,mTy,aTy]) <- Either.partitionEithers primArguments
  , (_:_:argTy:_) <- primArgTys rpc [nTy,mTy,aTy]
  = case (natSize rpc nTy, natSize rpc mTy) of
      (Right n, Right m) -> do
        shouldReduce1 <- List.orM [ pure (m==0)
                                  , shouldReduceHere rpc
                                  , isUntranslatableType_not_poly (rpcScope rpc) aTy
                                  --  Note [Unroll shouldSplit types]
                                  , pure (isSplit rpc argTy)
                                  ]
        if shouldReduce1
          then reduceWith rpc (ReduceUnconcat n m)
          else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 3
  = badArgs "unconcat" rpc
  | otherwise
  = return originalTerm

reduceTransposeHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceTransposeHandler rpc@ReducePrimContext{..}
  -- TODO(transpose): the type arguments of @transpose :: KnownNat n => Vec m
  -- (Vec n a) -> Vec n (Vec m a)@ are @n@, @m@ and @a@, in that order, so
  -- @mTy@ and @nTy@ are swapped here. This is long-standing behaviour, which
  -- is kept as is for now.
  | (_,[mTy,nTy,_aTy]) <- Either.partitionEithers primArguments
  = case (natSize rpc nTy, natSize rpc mTy) of
      (Right n, Right 0) -> reduceWith rpc (ReduceTranspose n)
      _ -> return originalTerm
  | length primArguments >= 3
  = badArgs "transpose" rpc
  | otherwise
  = return originalTerm

-- | The handler of @replicate@ and @replace_int@, of type @forall n a. ... ->
-- Vec n a@
reduceByResultHandler
  :: Distinct n
  => String
  -> Bool
  -- ^ Whether to unroll under @-fclash-ultra@
  -> (Integer -> PrimReduction)
  -> ReducePrimContext n
  -> NormalizeSession (Term n)
reduceByResultHandler nm whenUltra red rpc@ReducePrimContext{..}
  | (_,[nTy,aTy]) <- Either.partitionEithers primArguments
  = case natSize rpc nTy of
      Right n -> do
        shouldReduce1 <- List.orM [ pure (whenUltra && ultra)
                             , shouldReduceHere rpc
                             , isUntranslatableType_not_poly (rpcScope rpc) aTy
                             -- Note [Unroll shouldSplit types]
                             , pure (isSplit rpc resultType)
                             ]
        if shouldReduce1
           then reduceWith rpc (red n)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = badArgs nm rpc
  | otherwise
  = return originalTerm

reduceReplicateHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceReplicateHandler = reduceByResultHandler "replicate" False ReduceReplicate

-- replace_int :: KnownNat n => Vec n a -> Int -> a -> Vec n a
reduceReplaceIntHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceReplaceIntHandler = reduceByResultHandler "replace_int" True ReduceReplaceInt

reduceIndexIntHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceIndexIntHandler rpc@ReducePrimContext{..}
  | (_,[nTy,aTy]) <- Either.partitionEithers primArguments
  , (_:argTy:_) <- primArgTys rpc [nTy,aTy]
  = case natSize rpc nTy of
      Right n -> do
        shouldReduce1 <- List.orM [ pure ultra
                             , shouldReduceHere rpc
                             , isUntranslatableType_not_poly (rpcScope rpc) aTy
                             -- Note [Unroll shouldSplit types]
                             , pure (isSplit rpc argTy) ]
        if shouldReduce1
           then reduceWith rpc (ReduceIndexInt n)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = badArgs "index_int" rpc
  | otherwise
  = return originalTerm

reduceImapHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceImapHandler rpc@ReducePrimContext{..}
  | (_,[nTy,argElTy,resElTy]) <- Either.partitionEithers primArguments
  , TyConApp vecTcNm _ <- resultTypeView
  , let argTy = mkTyConApp vecTcNm [nTy,argElTy]
  = case natSize rpc nTy of
      Right n -> do
        shouldReduce1 <- List.orM [ pure (ultra || n < 2)
                             , shouldReduceHere rpc
                             , List.anyM (isUntranslatableType_not_poly (rpcScope rpc)) [argElTy,resElTy]
                             -- Note [Unroll shouldSplit types]
                             , pure (any (isSplit rpc) [argTy,resultType]) ]
        if shouldReduce1
           then reduceWith rpc (ReduceImap n)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 3
  = badArgs "imap" rpc
  | otherwise
  = return originalTerm

reduceIterateIHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceIterateIHandler rpc@ReducePrimContext{..}
  | (_,[nTy,aTy]) <- Either.partitionEithers primArguments
  = case natSize rpc nTy of
      Right n -> do
        shouldReduce1 <- List.orM
          [ pure (ultra || n < 2)
          , shouldReduceHere rpc
          , isUntranslatableType_not_poly (rpcScope rpc) aTy
          -- Note [Unroll shouldSplit types]
          , pure (isSplit rpc resultType) ]

        if shouldReduce1
          then reduceWith rpc (ReduceIterateI n)
          else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = badArgs "iterateI" rpc
  | otherwise
  = return originalTerm

reduceDTFoldHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceDTFoldHandler rpc@ReducePrimContext{..}
  | (_,[_mTy,nTy,_aTy]) <- Either.partitionEithers primArguments
  = case natSize rpc nTy of
      Right n -> reduceWith rpc (ReduceDTFold n)
      _ -> return originalTerm
  | length primArguments >= 3
  = badArgs "dtfold" rpc
  | otherwise
  = return originalTerm

reduceReverseHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceReverseHandler rpc@ReducePrimContext{..}
  | ultra
  , (_,[nTy,_aTy]) <- Either.partitionEithers primArguments
  , Right n <- natSize rpc nTy
  = reduceWith rpc (ReduceReverse n)
  | otherwise
  = return originalTerm

reduceTDFoldHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceTDFoldHandler rpc@ReducePrimContext{..}
  | (_,[_mTy,nTy,_aTy]) <- Either.partitionEithers primArguments
  = case natSize rpc nTy of
      Right n -> reduceWith rpc (ReduceTDFold n)
      _ -> return originalTerm
  | length primArguments >= 3
  = badArgs "tdfold" rpc
  | otherwise
  = return originalTerm

reduceTReplicateHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceTReplicateHandler rpc@ReducePrimContext{..}
  | (_,[nTy,aTy]) <- Either.partitionEithers primArguments
  = case natSize rpc nTy of
      Right n -> do
        shouldReduce1 <- List.orM [ shouldReduceHere rpc
                             , isUntranslatableType False (rpcScope rpc) aTy ]
        if shouldReduce1
           then reduceWith rpc (ReduceTReplicate n)
           else return originalTerm
      _ -> return originalTerm
  | length primArguments >= 2
  = badArgs "treplicate" rpc
  | otherwise
  = return originalTerm

reduceSplitHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceSplitHandler rpc@ReducePrimContext{..}
  | (tmArgs,[nTy,mTy]) <- Either.partitionEithers primArguments
  = case (natSize rpc nTy, natSize rpc mTy, resultTypeView) of
      (Right n, Right m, TyConApp tupTcNm [lTy,rTy])
        | n == 0 -> abstractOverMissingArgs 2 primTicks tmArgs termType transformContext $
            \_ctx args -> case args of
              [_kn, bvArg] -> do
                let tup = mkApps (Data tupDc)
                             [Right (sink lTy)
                             ,Right (sink rTy)
                             ,Left  bvArg
                             ,Left  (TyApp (Prim removedArg) (sink rTy))
                             ]
                changed (mkTicks tup (map sink primTicks))
              _ -> error "reducePrim: split# arity"
        | m == 0 -> abstractOverMissingArgs 2 primTicks tmArgs termType transformContext $
            \_ctx args -> case args of
              [_kn, bvArg] -> do
                let tup = mkApps (Data tupDc)
                             [Right (sink lTy)
                             ,Right (sink rTy)
                             ,Left  (TyApp (Prim removedArg) (sink lTy))
                             ,Left  bvArg
                             ]
                changed (mkTicks tup (map sink primTicks))
              _ -> error "reducePrim: split# arity"
       where
        tupDc = fromMaybe (error "reducePrim: faield to create tup DC") $ do
                tupTc <- lookupTyCon typeEnv tupTcNm
                listToMaybe (tyConDataCons tupTc)
      _ -> return originalTerm
  | length primArguments >= 3
  = badArgs "split#" rpc
  | otherwise
  = return originalTerm

reduceEqHandler :: Distinct n => ReducePrimContext n -> NormalizeSession (Term n)
reduceEqHandler rpc@ReducePrimContext{..}
  | (tmArgs,[nTy]) <- Either.partitionEithers primArguments
  , Right 0 <- natSize rpc nTy
  , TyConApp boolTcNm [] <- resultTypeView
  = abstractOverMissingArgs 3 primTicks tmArgs termType transformContext $ \_ctx _args ->
      let trueDc = fromMaybe (error "reducePrim: failed to create True DC") $ do
            boolTc <- lookupTyCon typeEnv boolTcNm
            [_falseDc,dc] <- pure (tyConDataCons boolTc)
            return dc
      in changed (Data trueDc)
  | otherwise
  = return originalTerm

isUntranslatableType_not_poly :: Distinct n => Scope n -> Type n -> NormalizeSession Bool
isUntranslatableType_not_poly sc t = do
  u <- isUntranslatableType False sc t
  if u
     then return (not (hasFreeVarsType t))
     else return False

-- | Abstract over a primitive until it is saturated: give the continuation
-- the given number of term arguments, abstracting over the ones that are
-- missing, and apply its result to the remaining ones.
--
-- The ticks of the primitive are put around the result of the continuation,
-- which lives in the scope extended with the missing arguments. They are not
-- sunk into that scope for the continuation: it gets them in the outer scope,
-- and has to 'sink' them if it uses them.
abstractOverMissingArgs
  :: forall n
   . (HasCallStack, Distinct n)
  => Int
  -- ^ The number of term arguments the continuation takes
  -> [TickInfo n]
  -- ^ Ticks originally tagged to the applied primitive
  -> [Term n]
  -- ^ Available arguments
  -> Type n
  -- ^ The type of the expression containing the applied primitive
  -> TransformContext n
  -- ^ The context in which reducePrim was called
  -> (forall l. (Distinct l, DExt n l) => TransformContext l -> [Term l] -> NormalizeSession (Term l))
  -> NormalizeSession (Term n)
abstractOverMissingArgs arity ticks args0 ty0 ctx0 f
  | length args0 >= arity =
      let (now, rest) = splitAt arity args0
      in (`mkTmApps` rest) . (`mkTicks` ticks) <$> f ctx0 now
  | otherwise = go ctx0 (arity - length args0) (reverse args0) ty0
 where
  -- The arguments gathered so far, in reverse order
  go
    :: forall x
     . (Distinct x, DExt n x)
    => TransformContext x -> Int -> [Term x] -> Type x -> NormalizeSession (Term x)
  go ctx 0 argsRev _ = (`mkTicks` map sink ticks) <$> f ctx (reverse argsRev)
  go ctx missing argsRev ty = case tyView ty of
    FunTy argTy resTy ->
      withFreshIdBinder (tfScope ctx) (C.mkUnsafeInternalName "arg" 0) argTy $ \(sc :: Scope l) b ->
        withExtTrans @n @x @l $
          let ctx1 = pushUnder sc (LamBody b) ctx
          in Lam b <$> go ctx1 (missing - 1) (idBinderVar b : map sink argsRev) (sink resTy)
    _ -> error ("not a funty: " <> showPpr ty)

-- * Running the reductions

-- | A reduction of "Clash.Normalize.Scoped.PrimitiveReductions", with the parameters
-- that the handlers in 'reducePrimImpls' decided on. The type arguments of the
-- reduction are those of the primitive, and the type of its result, so they
-- are not part of it. See 'runPrimReduction'.
data PrimReduction
  = ReduceZipWith !Integer !Bool !Bool
  -- ^ Length, whether the elements of the left and of the right argument are
  -- representable
  | ReduceMap !Integer !Bool
  -- ^ Length, whether the elements of the argument are representable
  | ReduceTraverse !Integer
  | ReduceFold !Integer
  | ReduceFoldr !Integer !Bool
  -- ^ Length, whether the elements are representable
  | ReduceDFold !Integer
  | ReduceAppend !Integer !Integer
  | ReduceHead !Integer
  | ReduceTail !Integer
  | ReduceLast !Integer
  | ReduceInit !Integer !Bool
  -- ^ Length, whether the elements are representable
  | ReduceUnconcat !Integer !Integer
  | ReduceTranspose !Integer
  | ReduceReplicate !Integer
  | ReduceReplaceInt !Integer
  | ReduceIndexInt !Integer
  | ReduceImap !Integer
  | ReduceIterateI !Integer
  | ReduceDTFold !Integer
  | ReduceReverse !Integer
  | ReduceTDFold !Integer
  | ReduceTReplicate !Integer
  deriving (Show)

-- | The number of term arguments of a reduction
primReductionArity :: PrimReduction -> Int
primReductionArity = \case
  ReduceZipWith {} -> 3
  ReduceMap {} -> 2
  ReduceTraverse {} -> 3
  ReduceFold {} -> 2
  ReduceFoldr {} -> 3
  ReduceDFold {} -> 5
  ReduceAppend {} -> 2
  ReduceHead {} -> 1
  ReduceTail {} -> 1
  ReduceLast {} -> 1
  ReduceInit {} -> 1
  ReduceUnconcat {} -> 3
  ReduceTranspose {} -> 2
  ReduceReplicate {} -> 2
  ReduceReplaceInt {} -> 4
  ReduceIndexInt {} -> 3
  ReduceImap {} -> 3
  ReduceIterateI {} -> 3
  ReduceDTFold {} -> 5
  ReduceReverse {} -> 1
  ReduceTDFold {} -> 5
  ReduceTReplicate {} -> 2

-- | Replace the primitive application of the context by a reduction,
-- abstracting over its missing arguments
reduceWith :: Distinct n => ReducePrimContext n -> PrimReduction -> NormalizeSession (Term n)
reduceWith ReducePrimContext{..} red =
  abstractOverMissingArgs (primReductionArity red) primTicks tmArgs termType transformContext $
    \ctx args -> runPrimReduction ctx red primInfo (map sink tyArgs) args
 where
  (tmArgs, tyArgs) = Either.partitionEithers primArguments

-- | Run a reduction of "Clash.Normalize.Scoped.PrimitiveReductions" on a
-- primitive, given its type arguments and as many term arguments as the
-- reduction takes ('primReductionArity').
runPrimReduction
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> PrimReduction
  -> PrimInfo
  -> [Type n]
  -- ^ Type arguments
  -> [Term n]
  -- ^ Term arguments
  -> NormalizeSession (Term n)
runPrimReduction ctx red p tys tms = do
  te <- Lens.view scopedTypeEnv
  let
    -- The type of the saturated application
    resTy = inferCoreTypeOf te (tfScope ctx) (mkApps (Prim p) (map Right tys <> map Left tms))
  case (red, tys, tms) of
    (ReduceZipWith n lRep rRep, [lhsElTy, rhsElTy, resElTy, _], [f, xs, ys]) ->
      PR.reduceZipWith p n lhsElTy rhsElTy resElTy lRep rRep f xs ys ctx
    (ReduceMap n argRep, [argElTy, resElTy, _], [f, xs]) ->
      PR.reduceMap p n argElTy resElTy argRep f xs ctx
    (ReduceTraverse n, [aTy, fTy, bTy, _], [dict, f, xs]) ->
      PR.reduceTraverse n aTy fTy bTy dict f xs ctx
    (ReduceFold n, [_, aTy], [f, xs]) ->
      PR.reduceFold n aTy f xs ctx
    (ReduceFoldr n aRep, [aTy, _, _], [f, z, xs]) ->
      PR.reduceFoldr p n aTy aRep f z xs ctx
    (ReduceDFold n, [_, _, aTy], [kn, proxy, f, z, xs]) ->
      PR.reduceDFold n aTy kn proxy f z xs ctx
    (ReduceAppend n m, [_, aTy, _], [xs, ys]) ->
      PR.reduceAppend n m aTy xs ys ctx
    (ReduceHead n, [_, aTy], [xs]) ->
      PR.reduceHead n aTy xs ctx
    (ReduceTail n, [_, aTy], [xs]) ->
      PR.reduceTail n aTy xs ctx
    (ReduceLast n, [_, aTy], [xs]) ->
      PR.reduceLast n aTy xs ctx
    (ReduceInit n aRep, [_, aTy], [xs]) ->
      PR.reduceInit p n aTy aRep xs ctx
    (ReduceUnconcat n m, [_, _, aTy], [kn, sm, xs]) ->
      PR.reduceUnconcat p n m aTy kn sm xs ctx
    (ReduceTranspose n, [_, _, aTy], [kn, xss]) ->
      PR.reduceTranspose n 0 aTy kn xss ctx
    (ReduceReplicate n, [_, aTy], [sn, x]) ->
      PR.reduceReplicate n aTy resTy sn x ctx
    (ReduceReplaceInt n, [_, aTy], [kn, xs, i, x]) ->
      PR.reduceReplace_int n aTy resTy kn xs i x ctx
    (ReduceIndexInt n, [_, aTy], [kn, xs, i]) ->
      PR.reduceIndex_int n aTy kn xs i ctx
    (ReduceImap n, [_, argElTy, resElTy], [kn, f, xs]) ->
      PR.reduceImap n argElTy resElTy kn f xs ctx
    (ReduceIterateI n, [_, aTy], [kn, f, x]) ->
      PR.reduceIterateI n aTy resTy kn f x ctx
    (ReduceDTFold n, [_, _, aTy], [kn, proxy, lrFun, brFun, xs]) ->
      PR.reduceDTFold n aTy kn proxy lrFun brFun xs ctx
    (ReduceReverse n, [_, aTy], [xs]) ->
      PR.reduceReverse n aTy xs ctx
    (ReduceTDFold n, [_, _, aTy], [kn, proxy, lrFun, brFun, t]) ->
      PR.reduceTFold n aTy kn proxy lrFun brFun t ctx
    (ReduceTReplicate n, [_, aTy], [sn, x]) ->
      PR.reduceTReplicate n aTy resTy sn x ctx
    _ ->
      error ("runPrimReduction: unexpected arguments for " <> show red <> ": "
             <> showPpr (mkApps (Prim p) (map Right tys <> map Left tms)))
