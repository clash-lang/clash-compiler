{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2022, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The X-optimization transformation.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Normalize.Transformations.XOptimize
  ( xOptimize
  ) where

import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Data.List.Extra as List
import qualified Data.Text.Extra as Text (showt)
import GHC.Stack (HasCallStack)

import Clash.XException (errorX)

import Clash.Annotations.Primitive (extractPrim)
import Clash.Core.DataCon (DataCon)
import Clash.Core.HasType
import Clash.Core.InScopeSet (InScopeSet)
import Clash.Core.Term
  ( Alt, IsMultiPrim(..), LetBinding, Pat(..), PrimInfo(..), Term(..)
  , WorkInfo(..), collectArgs, PrimUnfolding(..))
import Clash.Core.Type (TyVar, Type)
import Clash.Core.Util (mkInternalVar)
import Clash.Core.Var (Id)
import Clash.Netlist.BlackBox.Types (Element(Err))
import Clash.Netlist.Types (BlackBox(..))
import Clash.Normalize.Types (NormRewrite, NormalizeSession)
import Clash.Primitives.Types (Primitive(..))
import Clash.Rewrite.Types
  (TransformContext(..), aggressiveXOpt, tcCache, primitives)
import Clash.Rewrite.Util (changed)
import Clash.Unique (MonadUnique)
import Clash.Util (curLoc)

-- | Remove all undefined alternatives from case expressions, replacing them
-- with the value of another defined alternative. If there is one defined
-- alternative, the entire expression is replaced with that alternative. If
-- there are no defined alternatives, the entire expression is replaced with
-- a call to 'errorX'.
--
-- e.g. It converts
--
--     case x of
--       D1 a -> f a
--       D2   -> undefined
--       D3   -> undefined
--
-- to
--
--     let subj = x
--         a    = case subj of
--                  D1 a -> field0
--      in f a
--
-- where fieldN is an internal variable referring to the nth argument of a
-- data constructor.
--
xOptimize :: HasCallStack => NormRewrite
xOptimize (TransformContext is0 _) e@(Case subj ty alts) = do
  runXOpt <- Lens.view aggressiveXOpt

  if runXOpt then do
    defPart <- List.partitionM (isPrimError . snd) alts

    case defPart of
      ([], _)    -> return e
      (_, [])    -> changed (Prim (PrimInfo (Text.showt 'errorX) ty WorkConstant SingleResult NoUnfolding))
      (_, [alt]) -> xOptimizeSingle is0 subj alt
      (_, defs)  -> xOptimizeMany is0 subj ty defs
  else
    return e

xOptimize _ e = return e
{-# SCC xOptimize #-}

-- Return an expression equivalent to the alternative given. When only one
-- alternative is defined the result of this function is used to replace the
-- case expression.
--
xOptimizeSingle :: InScopeSet -> Term -> Alt -> NormalizeSession Term
xOptimizeSingle is subj (DataPat dc tvs vars, expr) = do
  tcm    <- Lens.view tcCache
  subjId <- mkInternalVar is "subj" (inferCoreTypeOf tcm subj)

  let fieldTys = fmap coreTypeOf vars
  lets <- Monad.zipWithM (mkFieldSelector is subjId dc tvs fieldTys) vars [0..]

  changed (Letrec ((subjId, subj) : lets) expr)

xOptimizeSingle _ _ (_, expr) = changed expr

-- Given a list of alternatives which are defined, create a new case
-- expression which only ever returns a defined value.
--
xOptimizeMany
  :: HasCallStack
  => InScopeSet
  -> Term
  -> Type
  -> [Alt]
  -> NormalizeSession Term
xOptimizeMany is subj ty defs@(d:ds)
  | isAnyDefault defs = changed (Case subj ty defs)
  | otherwise = do
      newAlt <- xOptimizeSingle is subj d
      changed (Case subj ty $ ds <> [(DefaultPat, newAlt)])
 where
  isAnyDefault :: [Alt] -> Bool
  isAnyDefault = any ((== DefaultPat) . fst)

xOptimizeMany _ _ _ [] =
  error $ $(curLoc) ++ "Report as bug: xOptimizeMany error: No defined alternatives"

mkFieldSelector
  :: MonadUnique m
  => InScopeSet
  -> Id
  -- ^ subject id
  -> DataCon
  -> [TyVar]
  -> [Type]
  -- ^ concrete types of fields
  -> Id
  -> Int
  -> m LetBinding
mkFieldSelector is0 subj dc tvs fieldTys nm index = do
  fields <- mapM (\ty -> mkInternalVar is0 "field" ty) fieldTys
  let alt = (DataPat dc tvs fields, Var $ fields !! index)
  return (nm, Case (Var subj) (fieldTys !! index) [alt])

-- Check whether a term is really a black box primitive representing an error.
-- Such values are undefined and are removed in X Optimization.
--
isPrimError :: Term -> NormalizeSession Bool
isPrimError (collectArgs -> (Prim pInfo, _)) = do
  prim <- Lens.view (primitives . Lens.at (primName pInfo))

  case prim >>= extractPrim of
    Just p  -> return (isErr p)
    Nothing -> return False
 where
  isErr BlackBox{template=(BBTemplate [Err _])} = True
  isErr _ = False

isPrimError _ = return False
