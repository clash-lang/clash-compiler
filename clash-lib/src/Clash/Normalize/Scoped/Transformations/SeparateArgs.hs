{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The separating arguments transformation of scoped Clash Core: the ported
  counterpart of "Clash.Normalize.Transformations.SeparateArgs".

  Unlike that of Clash Core, it doesn't split arguments of a product type of
  which the type of a field mentions an existentially quantified type
  variable, see 'Clash.Core.Scoped.Util.shouldSplit'.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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

import Clash.Core.HasType (coreTypeOf)
import Clash.Core.Name (nameOcc)
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.StrategyDSL
  ( Transformation, onAppNode, onLam, onTickNode, onTyAppNode, onVarNode
  , toScopedTransformation )
import Clash.Rewrite.Types (scopedTypeEnv)

import Clash.Core.Scoped.HasType (inferCoreTypeOf)
import Clash.Core.Scoped.Subst
  (extendSubstScope, extendTmSubst, idSubst, substTm, withFreshIdBinders)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (collectArgsTicks, mkApps, mkLams, mkTicks)
import Clash.Core.Scoped.Util (SplitInfo (..), shouldSplit)
import Clash.Rewrite.Scoped (TransformContext, changed, mkDerivedName, tfScope)

-- | Split apart (global) function arguments that contain types that we want
-- to separate off, e.g. Clocks. See
-- 'Clash.Normalize.Transformations.SeparateArgs.separateArguments'.
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
  te <- Lens.view scopedTypeEnv
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
-- account as 'Clash.Normalize.Transformations.SeparateArgs.separateArgumentsSpineWorker'
-- does: on its closed Clash Core type.
separateArgumentsSpineWorker
  :: forall n
   . (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (Term n)
separateArgumentsSpineWorker ctx e
  | (GlobalVar g, args, ticks) <- collectArgsTicks e = do
      let gv = globalIdVar g
          (argTys0, resTy) = C.splitFunForallTy (coreTypeOf gv)
      (concat -> args1, Monoid.getAny -> hasChanged)
        <- listen (mapM (uncurry splitArg) (zip argTys0 args))
      if hasChanged then
        let (argTys1, args2) = unzip args1
            g' = mkGlobalId gv{C.varType = C.mkPolyFunTy resTy argTys1}
        in  return (mkApps (mkTicks (GlobalVar g') ticks) args2)
      else
        return e
  | otherwise = return e
 where
  sc = tfScope ctx

  -- Split a single argument, given the quantifier or function argument type of
  -- the global variable
  splitArg
    :: Either C.TyVar C.Type
    -> Either (Term n) (Type n)
    -> NormalizeSession [(Either C.TyVar C.Type, Either (Term n) (Type n))]
  splitArg ty arg = case arg of
    Right _ -> return [(ty, arg)]
    Left tmArg -> do
      te <- Lens.view scopedTypeEnv
      case shouldSplit te sc (inferCoreTypeOf te sc tmArg) of
        Just info -> changed (map ((ty,) . Left) (splitProject info sc tmArg))
        Nothing -> return [(ty, arg)]
{-# SCC separateArgumentsSpineWorker #-}
