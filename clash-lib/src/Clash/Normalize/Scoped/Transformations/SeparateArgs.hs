{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The separating arguments transformation of scoped Clash Core.

  It doesn't split arguments of a product type of which the type of a field
  mentions an existentially quantified type variable, see
  'Clash.Core.Scoped.Util.shouldSplit'.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Normalize.Scoped.Transformations.SeparateArgs
  ( separateArguments
  , separateArgumentsLambdaWorker
  , separateArgumentsSpineWorker
  ) where

import qualified Control.Lens as Lens
import Control.Monad.Writer (listen)
import qualified Data.Monoid as Monoid
import GHC.Stack (HasCallStack)

import Clash.Core.Name (nameOcc)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.StrategyDSL
  ( Transformation, onAppNode, onLam, onTickNode, onTyAppNode, onVarNode
  , toScopedTransformation )
import Clash.Rewrite.Types (typeEnv)

import Clash.Core.Scoped.HasType (inferCoreTypeOf)
import Clash.Core.Scoped.Subst
  (closeType, extendSubstScope, extendTmSubst, idSubst, openTyBinder, substTm, withFreshIdBinders)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (collectArgsTicks, mkApps, mkLams, mkTicks)
import Clash.Core.Scoped.Type
  (SplitFunForall (..), TypeView (..), mkFunTy, piTeleTmArity, piTeleTyArity, splitFunForallTy, tyView)
import Clash.Core.Scoped.Util (SplitInfo (..), shouldSplit)
import Clash.Rewrite.Scoped (TransformContext, changed, mkDerivedName, tfScope)

-- | Split apart (global) function arguments that contain types that we want
-- to separate off, e.g. Clocks. Works on both the definition side (i.e. the
-- lambda), and the call site (i.e. the application of the global variable).
-- e.g. turns
--
-- > f :: (Clock System, Reset System) -> Signal System Int
--
-- into
--
-- > f :: Clock System -> Reset System -> Signal System Int
separateArguments :: Transformation
separateArguments = toScopedTransformation "separateArguments"
  (  onLam 'separateArgumentsLambdaWorker
  <> onVarNode 'separateArgumentsSpineWorker
  <> onAppNode 'separateArgumentsSpineWorker
  <> onTyAppNode 'separateArgumentsSpineWorker
  <> onTickNode 'separateArgumentsSpineWorker)

-- | The 'Lam' handler of 'separateArguments': splits the binder of a lambda
-- into binders for the fields of its type
separateArgumentsLambdaWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> IdBinder n l
  -> Term l
  -> NormalizeSession (Term n)
separateArgumentsLambdaWorker ctx node b eb = do
  te <- Lens.view typeEnv
  case shouldSplit te sc (idBinderType b) of
    Just info ->
      let nm = mkDerivedName ctx (nameOcc (idBinderOcc b))
      in  withFreshIdBinders sc [(nm, ty) | ty <- splitArgTys info] $ \sc' bs vs ->
            let s = extendTmSubst (extendSubstScope sc' (idSubst sc)) b (splitConstruct info sc' vs)
            in  changed (mkLams (substTm s eb) bs)
    Nothing -> return node
 where
  sc = tfScope ctx
{-# SCC separateArgumentsLambdaWorker #-}

-- | The application spine ('Var', 'App', 'TyApp', 'Tick') handler of
-- 'separateArguments': splits the arguments of a global function.
--
-- It updates the type of the global variable to take the split arguments into
-- account, and applies the global function to the split apart arguments. The
-- global can be applied to fewer arguments than it has parameters, whose
-- types are kept, or to more, when its result type is a type variable
-- instantiated with a function type, which stay applied. The type of a global
-- is closed, so it only splits an argument whose parts have closed types.
separateArgumentsSpineWorker
  :: forall n
   . (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (Term n)
separateArgumentsSpineWorker ctx e
  | (GlobalVar g, args, ticks) <- collectArgsTicks e = do
      let arity = case splitFunForallTy (globalIdType g) of
            SplitFunForall tele _ -> piTeleTyArity tele + piTeleTmArity tele
      (splits, Monoid.getAny -> hasChanged)
        <- listen (mapM splitArg (take arity args))
      if hasChanged then
        let gTy = splitGlobalType emptyScope (globalIdType g) (map fst splits)
            args1 = concatMap snd splits ++ drop arity args
            g' = mkGlobalId (globalIdName g) gTy
        in  return (mkApps (mkTicks (GlobalVar g') ticks) args1)
      else
        return e
  | otherwise = return e
 where
  sc = tfScope ctx

  -- Split a single argument: the types of its parts, if it is split, and the
  -- arguments that replace it
  splitArg
    :: Either (Term n) (Type n)
    -> NormalizeSession (Maybe [Type 'VoidS], [Either (Term n) (Type n)])
  splitArg arg = case arg of
    Right _ -> return (Nothing, [arg])
    Left tmArg -> do
      te <- Lens.view typeEnv
      case shouldSplit te sc (inferCoreTypeOf te sc tmArg) of
        Just info
          | Just tys <- traverse closeType (splitArgTys info) ->
              changed (Just tys, map Left (splitProject info sc tmArg))
        _ -> return (Nothing, [arg])
{-# SCC separateArgumentsSpineWorker #-}

-- | The type of a global whose arguments are split: per quantified type
-- variable or argument type, the closed types of the parts of the argument,
-- if it is split
splitGlobalType :: Distinct x => Scope x -> Type x -> [Maybe [Type 'VoidS]] -> Type x
splitGlobalType _ ty [] = ty
splitGlobalType sc ty (split : splits) = case ty of
  ForAllTy b res ->
    openTyBinder sc b res $ \sc' b' res' -> ForAllTy b' (splitGlobalType sc' res' splits)
  (tyView -> FunTy arg res) ->
    let res' = splitGlobalType sc res splits
    in case split of
         Nothing -> mkFunTy arg res'
         Just tys -> foldr (mkFunTy . sinkClosed) res' tys
  _ -> ty
