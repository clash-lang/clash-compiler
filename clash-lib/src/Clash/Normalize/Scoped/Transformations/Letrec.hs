{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations on let-expressions of scoped Clash Core: the ported
  counterparts of "Clash.Normalize.Transformations.Letrec".
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Scoped.Transformations.Letrec
  ( deadCode
  , deadCodeWorker
  , removeUnusedExpr
  , removeUnusedExprCase
  , removeUnusedExprSpine
  ) where

import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Writer (listen)
import qualified Data.Either as Either
import qualified Data.HashMap.Lazy as HashMap
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import qualified Data.Monoid as Monoid (Any (..))
import qualified Data.Text.Extra as Text
import GHC.Stack (HasCallStack)

import Clash.Annotations.BitRepresentation.Deriving (dontApplyInHDL)
import Clash.Sized.Vector as Vec (Vec (Cons), splitAt)

import Clash.Annotations.Primitive (extractPrim)
import Clash.Core.DataCon (DataCon (..))
import qualified Clash.Core.HasType as C (coreTypeOf)
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Core.Type as C
import Clash.Netlist.BlackBox.Types ()
import Clash.Netlist.BlackBox.Util (getUsedArguments)
import Clash.Normalize.Primitives (removedArg)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Primitives.Types (Primitive (..), UsedArguments (..))
import Clash.Rewrite.StrategyDSL
  ( Transformation, onAppNode, onCase, onLet, onPrimNode, onTickNode
  , onTyAppNode, toScopedTransformation )
import Clash.Rewrite.Types (primitives, scopedTypeEnv)

import Clash.Core.Scoped.HasType (inferCoreTypeOf)
import Clash.Core.Scoped.Subst (unsinkAlt)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (collectArgs, collectArgsTicks, mkApps, mkTicks)
import Clash.Core.Scoped.TermInfo (isCon)
import Clash.Core.Scoped.Type (TypeView (..), tyNatSize, tyView)
import Clash.Core.Scoped.TypeEnv (TypeEnv, lookupTyCon)
import Clash.Core.Scoped.Util (mkVec)
import Clash.Rewrite.Scoped (FrameKind (..), TransformContext, changed, ctxFrames, tfScope)
import Clash.Rewrite.Scoped.Util (isFromInt, removeUnusedBinders)

-- | Remove unused let-bindings
deadCode :: Transformation
deadCode = toScopedTransformation "deadCode" (onLet 'deadCodeWorker)

-- | The 'Let' handler of 'deadCode'
deadCodeWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Bind n l
  -> Term l
  -> NormalizeSession (Term n)
deadCodeWorker ctx e bind body =
  case removeUnusedBinders (tfScope ctx) bind body of
    Just t -> changed t
    Nothing -> return e
{-# SCC deadCodeWorker #-}

-- * removeUnusedExpr

-- | Replace the unused arguments of primitives by 'removedArg', see
-- 'Clash.Normalize.Transformations.Letrec.removeUnusedExpr'
removeUnusedExpr :: Transformation
removeUnusedExpr = toScopedTransformation "removeUnusedExpr"
  (  onPrimNode 'removeUnusedExprSpine
  <> onAppNode 'removeUnusedExprSpine
  <> onTyAppNode 'removeUnusedExprSpine
  <> onTickNode 'removeUnusedExprSpine
  <> onCase 'removeUnusedExprCase )

-- | The 'Case' handler of 'removeUnusedExpr': a single-alternative case whose
-- pattern binds no used variables is replaced by the alternative.
removeUnusedExprCase
  :: TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> [Alt n]
  -> NormalizeSession (Term n)
removeUnusedExprCase _ e _subj _ty [Alt p@(DataPat _ TNil _) altExpr] =
  case unsinkAlt p altExpr of
    Just altExpr1 -> changed altExpr1
    Nothing -> return e
removeUnusedExprCase _ e _subj _ty _alts = return e

-- | The application-spine handlers of 'removeUnusedExpr': replace unused
-- arguments of primitives (and 'Vec's 'Vec.Cons') with 'removedArg'. See
-- 'Clash.Normalize.Transformations.Letrec.removeUnusedExprSpine' for why only
-- the root of a spine is rewritten.
removeUnusedExprSpine
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (Term n)
removeUnusedExprSpine ctx e = case ctxFrames ctx of
  KAppFun : _ -> return e
  KTyAppC : _ -> return e
  KTickC : _ -> return e
  _ -> case collectArgsTicks e of
    (p@(Prim pInfo), args, ticks) -> removeUnusedPrimArgs sc e p pInfo args ticks
    -- Replace any expression that creates a Vector of size 0 within the
    -- application of the Cons constructor, by the Nil constructor.
    (Data dc, [_, Right aTy, Right nTy, _, Left a, Left nil], ticks)
      | C.nameOcc (dcName dc) == Text.showt 'Vec.Cons -> do
          te <- Lens.view scopedTypeEnv
          case runExcept (tyNatSize te sc nTy) of
            Right 0
              | (con, _) <- collectArgs nil
              , not (isCon con)
              -> let eTy = inferCoreTypeOf te sc e
                     v = fromMaybe (error "removeUnusedExpr: failed to create Vec DCs") $ do
                       TyConApp vecTcNm _ <- pure (tyView eTy)
                       vecTc <- lookupTyCon te vecTcNm
                       [nilCon, consCon] <- pure (tyConDataCons vecTc)
                       return (mkTicks (mkVec te sc nilCon consCon aTy 1 [a]) ticks)
                 in  changed v
            _ -> return e
    _ -> return e
 where
  sc = tfScope ctx
{-# SCC removeUnusedExprSpine #-}

-- | Replace the unused arguments of an applied primitive by 'removedArg'
removeUnusedPrimArgs
  :: forall n
   . (HasCallStack, Distinct n)
  => Scope n
  -> Term n
  -- ^ The application
  -> Term n
  -- ^ The primitive
  -> C.PrimInfo
  -> [Either (Term n) (Type n)]
  -> [TickInfo n]
  -> NormalizeSession (Term n)
removeUnusedPrimArgs sc e p pInfo args ticks = do
  bbM <- HashMap.lookup (C.primName pInfo) <$> Lens.view primitives
  let
    usedArgs0 =
      case Monad.join (extractPrim <$> bbM) of
        Just (BlackBoxHaskell{usedArguments}) ->
          case usedArguments of
            UsedArguments used -> Just used
            IgnoredArguments ignored -> Just ([0..length args - 1] \\ ignored)
        Just (BlackBox pNm _ _ _ _ _ _ _ _ _ inc r ri templ) -> Just $
          if | isFromInt pNm -> [0,1,2]
             | C.primName pInfo `elem` [ Text.showt 'dontApplyInHDL
                                       , Text.showt 'Vec.splitAt
                                       ] -> [0,1]
             | otherwise -> concat [ concatMap getUsedArguments r
                                   , concatMap getUsedArguments ri
                                   , getUsedArguments templ
                                   , concatMap (getUsedArguments . snd) inc ]
        _ ->
          Nothing

  case usedArgs0 of
    Nothing ->
      return e
    Just usedArgs1 -> do
      te <- Lens.view scopedTypeEnv
      (args1, Monoid.getAny -> hasChanged) <- listen (go te 0 usedArgs1 args)
      if hasChanged then
        return (mkApps (mkTicks p ticks) args1)
      else
        return e
 where
  -- The type of a primitive is closed, so this uses the one of Clash Core
  arity = length . Either.rights . fst $ C.splitFunForallTy (C.coreTypeOf pInfo)

  go :: TypeEnv -> Int -> [Int] -> [Either (Term n) (Type n)]
     -> NormalizeSession [Either (Term n) (Type n)]
  go _ _ _ [] = return []
  go te !n used (Right ty:args') = do
    args'' <- go te n used args'
    return (Right ty : args'')
  go te !n used (Left tm : args') = do
    args'' <- go te (n+1) used args'
    case tm of
      TyApp (Prim p0) _
        | C.primName p0 == Text.showt 'removedArg
        -> return (Left tm : args'')
      _ -> do
        let ty = inferCoreTypeOf te sc tm
            p' = TyApp (Prim removedArg) ty
        if n < arity && n `notElem` used
           then changed (Left p' : args'')
           else return  (Left tm : args'')
