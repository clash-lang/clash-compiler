{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021     , QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
  The separating arguments transformation
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Normalize.Transformations.SeparateArgs
  ( separateArguments
  ) where

import qualified Control.Lens as Lens
import Control.Monad.Writer (listen)
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import GHC.Stack (HasCallStack)

import Clash.Core.HasType
import qualified Clash.Core.InScopeSet as InScopeSet
import Clash.Core.Name (Name(..))
import Clash.Core.Subst (extendIdSubst, mkSubst, substTm)
import Clash.Core.Term (Term(..), collectArgsTicks, mkApps, mkLams, mkTicks)
import Clash.Core.Type (Type, mkPolyFunTy, splitFunForallTy)
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Util (Projections (..), shouldSplit)
import Clash.Core.Var (Id, TyVar, Var (..), isGlobalId, mkLocalId)
import Clash.Normalize.Types (NormRewrite, NormalizeSession)
import Clash.Rewrite.Types (TransformContext(..), tcCache)
import Clash.Rewrite.Util (changed, mkDerivedName)

-- | Split apart (global) function arguments that contain types that we
-- want to separate off, e.g. Clocks. Works on both the definition side (i.e. the
-- lambda), and the call site (i.e. the application of the global variable). e.g.
-- turns
--
-- > f :: (Clock System, Reset System) -> Signal System Int
--
-- into
--
-- > f :: Clock System -> Reset System -> Signal System Int
separateArguments :: HasCallStack => NormRewrite
separateArguments ctx e0@(Lam b eb) = do
  tcm <- Lens.view tcCache
  case separateLambda tcm ctx b eb of
    Just e1 -> changed e1
    Nothing -> return e0

separateArguments (TransformContext is0 _) e@(collectArgsTicks -> (Var g, args, ticks))
  | isGlobalId g = do
  -- We ensure that both the type of the global variable reference is updated
  -- to take into account the changed arguments, and that we apply the global
  -- function with the split apart arguments.
  let (argTys0,resTy) = splitFunForallTy (coreTypeOf g)
  (concat -> args1, Monoid.getAny -> hasChanged)
    <- listen (mapM (uncurry splitArg) (zip argTys0 args))
  if hasChanged then
    let (argTys1,args2) = unzip args1
        gTy = mkPolyFunTy resTy argTys1
    in  return (mkApps (mkTicks (Var g {varType = gTy}) ticks) args2)
  else
    return e

 where
  -- Split a single argument
  splitArg
    :: Either TyVar Type
    -- The quantifier/function argument type of the global variable
    -> Either Term Type
    -- The applied type argument or term argument
    -> NormalizeSession [(Either TyVar Type,Either Term Type)]
  splitArg tv arg@(Right _)    = return [(tv,arg)]
  splitArg ty arg@(Left tmArg) = do
    tcm <- Lens.view tcCache
    let argTy = inferCoreTypeOf tcm tmArg
    case shouldSplit tcm argTy of
      Just (_,Projections projections,_) -> do
        tmArgs <- projections is0 tmArg
        changed (map ((ty,) . Left) tmArgs)
      _ ->
        return [(ty,arg)]

separateArguments _ e = return e
{-# SCC separateArguments #-}

-- | Worker function of 'separateArguments'.
separateLambda
  :: TyConMap
  -> TransformContext
  -> Id
  -- ^ Lambda binder
  -> Term
  -- ^ Lambda body
  -> Maybe Term
  -- ^ If lambda is split up, this function returns a Just containing the new term
separateLambda tcm ctx@(TransformContext is0 _) b eb0 =
  case shouldSplit tcm (coreTypeOf b) of
    Just (dc, _, argTys) ->
      let
        nm = mkDerivedName ctx (nameOcc (varName b))
        bs0 = map (`mkLocalId` nm) argTys
        (is1, bs1) = List.mapAccumL newBinder is0 bs0
        subst = extendIdSubst (mkSubst is1) b (dc (map Var bs1))
        eb1 = substTm "separateArguments" subst eb0
      in
        Just (mkLams eb1 bs1)
    _ ->
      Nothing
 where
  newBinder isN0 x =
    let
      x' = InScopeSet.uniqAway isN0 x
      isN1 = InScopeSet.insert x' isN0
    in
      (isN1, x')
{-# SCC separateLambda #-}
