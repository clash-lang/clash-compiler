{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The X-optimization transformation of scoped Clash Core: the ported
  counterpart of "Clash.Normalize.Transformations.XOptimize".

  When only one alternative is defined, 'xOptimize' let-binds the subject and
  the pattern binders. It creates new binders for them in the scope of the
  case-expression, so the pattern binders can't capture the free variables of
  the subject, and the binder of the subject is fresh with respect to them:
  bugs of the transformation of Clash Core.

  When no alternative is defined, it replaces the case-expression with
  @errorX@. The type of a primitive is closed in scoped Core, so if the type
  of the case-expression mentions type variables, that is @errorX \@ty@, where
  @errorX@ has type @forall a. a@ (see 'Clash.Core.Scoped.Util.primAtType'),
  where Clash Core gives @errorX@ the open type.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Scoped.Transformations.XOptimize
  ( xOptimize
  , xOptimizeWorker
  ) where

import qualified Control.Lens as Lens
import qualified Data.List.Extra as List
import qualified Data.Text.Extra as Text (showt)
import GHC.Stack (HasCallStack)

import Clash.XException (errorX)

import Clash.Annotations.Primitive (extractPrim)
import qualified Clash.Core.Term as C
import Clash.Core.Scoped.TysPrim (undefinedTy)
import Clash.Core.DataCon (DataCon)
import Clash.Netlist.BlackBox.Types (Element (Err))
import Clash.Netlist.Types (BlackBox (..))
import Clash.Normalize.Types (NormalizeSession)
import Clash.Primitives.Types (Primitive (..))
import Clash.Rewrite.StrategyDSL (Transformation, onCase, toScopedTransformation)
import Clash.Rewrite.Types (aggressiveXOpt, primitives, scopedTypeEnv)
import Clash.Util (curLoc)

import Clash.Core.Scoped.HasType (inferCoreTypeOf)
import Clash.Core.Scoped.Subst
  ( extendSubstScope, idSubst, substIdBinder, substScope, substTele, substTm
  , teleIdBinderTypes, withFreshIdBinder, withFreshIdBinders )
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Convert (mkPrimInfo)
import Clash.Core.Scoped.Term (collectArgs)
import Clash.Core.Scoped.Util (internalName, primAtType)
import Clash.Rewrite.Scoped (TransformContext, changed, tfScope)

-- | Remove all undefined alternatives from case expressions, replacing them
-- with the value of another defined alternative, see
-- 'Clash.Normalize.Transformations.XOptimize.xOptimize'
xOptimize :: Transformation
xOptimize = toScopedTransformation "xOptimize" (onCase 'xOptimizeWorker)

-- | The 'Case' handler of 'xOptimize'
xOptimizeWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Type n
  -> [Alt n]
  -> NormalizeSession (Term n)
xOptimizeWorker ctx node subj ty alts = do
  runXOpt <- Lens.view aggressiveXOpt

  if runXOpt then do
    defPart <- List.partitionM (\(Alt _ e) -> isPrimError e) alts

    case defPart of
      ([], _) -> return node
      (_, []) -> changed (errorXOf ty)
      (_, [alt]) -> xOptimizeSingle sc subj ty alt
      (_, defs) -> xOptimizeMany sc subj ty defs
  else
    return node
 where
  sc = tfScope ctx
{-# SCC xOptimizeWorker #-}

-- | @errorX@ at a type, see the module header
errorXOf :: Type n -> Term n
errorXOf = primAtType
  (mkPrimInfo (Text.showt 'errorX) undefinedTy C.WorkConstant C.SingleResult NoUnfolding)

-- | An expression equivalent to the alternative given. When only one
-- alternative is defined, this replaces the case-expression.
--
-- We can't let-bind the type variables bound by the pattern, so we keep the
-- case-expression for patterns with existential type variables.
xOptimizeSingle
  :: (HasCallStack, Distinct n)
  => Scope n
  -> Term n
  -> Type n
  -> Alt n
  -> NormalizeSession (Term n)
xOptimizeSingle sc subj ty alt = case alt of
  Alt (DataPat _ (TCons _ _) _) _ -> changed (Case subj ty [alt])
  Alt (DataPat dc TNil ids) expr -> do
    te <- Lens.view scopedTypeEnv
    let fieldTys = teleIdBinderTypes ids
    -- letrec subj' = subj; x1 = case subj' of dc f1 .. fk -> f1; ... in expr
    changed $
      withFreshIdBinder sc (internalName "subj") (inferCoreTypeOf te sc subj) $ \sc1 subjB ->
        substTele substIdBinder (extendSubstScope sc1 (idSubst sc)) ids $ \s ids' ->
          let subjV = sink (idBinderVar subjB)
              sels = [ mkFieldSelector (substScope s) subjV dc (map sink fieldTys) i
                     | i <- [0 .. length fieldTys - 1] ]
          in  Let (Rec (TCons subjB ids') (sink subj : sels)) (substTm s expr)
  Alt (LitPat _) expr -> changed expr
  Alt DefaultPat expr -> changed expr

-- | Given a list of alternatives which are defined, create a new case
-- expression which only ever returns a defined value
xOptimizeMany
  :: (HasCallStack, Distinct n)
  => Scope n
  -> Term n
  -> Type n
  -> [Alt n]
  -> NormalizeSession (Term n)
xOptimizeMany sc subj ty defs = case defs of
  d : ds
    | any isDefault defs -> changed (Case subj ty defs)
    | otherwise -> do
        newAlt <- xOptimizeSingle sc subj ty d
        changed (Case subj ty (ds <> [Alt DefaultPat newAlt]))
  [] -> error $ $(curLoc) ++ "Report as bug: xOptimizeMany error: No defined alternatives"
 where
  isDefault (Alt DefaultPat _) = True
  isDefault _ = False

-- | @case subj of dc f0 .. fk -> fi@, for the @i@-th field of a data
-- constructor without existential type variables
mkFieldSelector
  :: Distinct n
  => Scope n
  -> Term n
  -- ^ The subject
  -> DataCon
  -> [Type n]
  -- ^ The types of the fields
  -> Int
  -> Term n
mkFieldSelector sc subj dc fieldTys index =
  withFreshIdBinders sc [(internalName "field", t) | t <- fieldTys] $ \_ fields vars ->
    Case subj (fieldTys !! index) [Alt (DataPat dc TNil fields) (vars !! index)]

-- | Check whether a term is really a black box primitive representing an
-- error. Such values are undefined and are removed in X Optimization.
isPrimError :: Term l -> NormalizeSession Bool
isPrimError e = case collectArgs e of
  (Prim pInfo, _) -> do
    prim <- Lens.view (primitives . Lens.at (primName pInfo))
    case prim >>= extractPrim of
      Just p -> return (isErr p)
      Nothing -> return False
  _ -> return False
 where
  isErr BlackBox{template = BBTemplate [Err _]} = True
  isErr _ = False
