{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The current implementations of transformations that the normalizer runs
  ported versions of: porting a transformation swaps the one that the
  strategies use (see "Clash.Rewrite.Scoped"), but leaves the worker of the
  current one in place until Phase 7 of the port.
-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Current
  ( deadCode
  , caseCase
  , caseCon
  , caseEliminateNonReachable
  , caseFlat
  , caseLet
  , disjointExpressionConsolidation
  , elimCaseBigNumInternals
  , elimExistentials
  , flattenLet
  , nonRepANF
  , recToLetRec
  , reduceConst
  , reducePrim
  , removeUnusedExpr
  , simpleCSE
  , topLet
    -- * Inline
  , bindConstantVar
  , inlineCast
  , inlineCleanup
  , collapseRHSNoops
  , inlineOrLiftNonRep
  , inlineSimIO
  , inlineSmall
  , inlineWorkFree
  , inlineNonRep
    -- * Specialize
  , appProp
  , constantSpec
  , nonRepSpec
  , typeSpec
  , zeroWidthSpec
    -- * Cast
  , argCastSpec
  ) where

import qualified Clash.Normalize.Transformations.ANF as ANF
import qualified Clash.Normalize.Transformations.Case as Case
import qualified Clash.Normalize.Transformations.Cast as Cast
import qualified Clash.Normalize.Transformations.DEC as DEC
import qualified Clash.Normalize.Transformations.Inline as Inline
import qualified Clash.Normalize.Transformations.Letrec as Letrec
import qualified Clash.Normalize.Transformations.Reduce as Reduce
import qualified Clash.Normalize.Transformations.Specialize as Specialize
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL
  ( Transformation, anyConstructor, onApp, onAppNode, onCase, onLam, onLet, onLetNode
  , onPrimNode, onTickNode, onTyApp, onTyAppNode, onVarNode, toTransformation )

-- | The current 'Clash.Normalize.Transformations.Letrec.deadCode', which
-- 'Clash.Normalize.Scoped.Transformations.Letrec.deadCode' replaced
deadCode :: Transformation
deadCode = toTransformation "deadCode" (onLet 'Letrec.deadCodeWorker)

-- | The current 'Clash.Normalize.Transformations.DEC.disjointExpressionConsolidation',
-- which 'Clash.Normalize.Scoped.Transformations.DEC.disjointExpressionConsolidation'
-- replaced
disjointExpressionConsolidation :: Transformation
disjointExpressionConsolidation =
  toTransformation "DEC" (onCase 'DEC.disjointExpressionConsolidationWorker)

-- | The current 'Clash.Normalize.Transformations.Case.caseCase', which
-- 'Clash.Normalize.Scoped.Transformations.Case.caseCase' replaced
caseCase :: Transformation
caseCase = toTransformation "caseCase" (onCase 'Case.caseCaseWorker)

-- | The current 'Clash.Normalize.Transformations.Case.caseCon', which
-- 'Clash.Normalize.Scoped.Transformations.Case.caseCon' replaced
caseCon :: Transformation
caseCon = toTransformation "caseCon" (onCase 'Case.caseConWorker)

-- | The current 'Clash.Normalize.Transformations.Case.caseEliminateNonReachable',
-- which 'Clash.Normalize.Scoped.Transformations.Case.caseEliminateNonReachable'
-- replaced
caseEliminateNonReachable :: Transformation
caseEliminateNonReachable =
  toTransformation "caseEliminateNonReachable" (onCase 'Case.caseEliminateNonReachableWorker)

-- | The current 'Clash.Normalize.Transformations.Case.caseFlat', which
-- 'Clash.Normalize.Scoped.Transformations.Case.caseFlat' replaced
caseFlat :: Transformation
caseFlat = toTransformation "caseFlat" (onCase 'Case.caseFlatWorker)

-- | The current 'Clash.Normalize.Transformations.Case.caseLet', which
-- 'Clash.Normalize.Scoped.Transformations.Case.caseLet' replaced
caseLet :: Transformation
caseLet = toTransformation "caseLet" (onCase 'Case.caseLetWorker)

-- | The current 'Clash.Normalize.Transformations.Case.elimExistentials', which
-- 'Clash.Normalize.Scoped.Transformations.Case.elimExistentials' replaced
elimExistentials :: Transformation
elimExistentials =
  toTransformation "elimExistentials" (onCase 'Case.elimExistentialsWorker)

-- | The current 'Clash.Normalize.Transformations.Case.elimCaseBigNumInternals',
-- which 'Clash.Normalize.Scoped.Transformations.Case.elimCaseBigNumInternals'
-- replaced
elimCaseBigNumInternals :: Transformation
elimCaseBigNumInternals =
  toTransformation "elimCaseBigNum" (onCase 'Case.elimCaseBigNumInternalsWorker)

-- | The current 'Clash.Normalize.Transformations.ANF.nonRepANF', which
-- 'Clash.Normalize.Scoped.Transformations.ANF.nonRepANF' replaced
nonRepANF :: Transformation
nonRepANF = toTransformation "nonRepANF" (onApp 'ANF.nonRepANFWorker)

-- | The current 'Clash.Normalize.Transformations.Reduce.reduceConst', which
-- 'Clash.Normalize.Scoped.Transformations.Reduce.reduceConst' replaced
reduceConst :: Transformation
reduceConst = toTransformation "reduceConst" (onApp 'Reduce.reduceConstWorker)

-- | The current 'Clash.Normalize.Transformations.Reduce.reducePrim', which
-- 'Clash.Normalize.Scoped.Transformations.Reduce.reducePrim' replaced
reducePrim :: Transformation
reducePrim = toTransformation "reducePrim" (onApp 'Reduce.reducePrimWorker)

-- | The current 'Clash.Normalize.Transformations.Letrec.removeUnusedExpr',
-- which 'Clash.Normalize.Scoped.Transformations.Letrec.removeUnusedExpr'
-- replaced
removeUnusedExpr :: Transformation
removeUnusedExpr = toTransformation "removeUnusedExpr"
  (  onPrimNode 'Letrec.removeUnusedExprSpine
  <> onAppNode 'Letrec.removeUnusedExprSpine
  <> onTyAppNode 'Letrec.removeUnusedExprSpine
  <> onTickNode 'Letrec.removeUnusedExprSpine
  <> onCase 'Letrec.removeUnusedExprCase)

-- | The current 'Clash.Normalize.Transformations.Letrec.flattenLet', which
-- 'Clash.Normalize.Scoped.Transformations.Letrec.flattenLet' replaced
flattenLet :: Transformation
flattenLet = toTransformation "flattenLet" (onLetNode 'Letrec.flattenLetWorker)

-- | The current 'Clash.Normalize.Transformations.Letrec.topLet', which
-- 'Clash.Normalize.Scoped.Transformations.Letrec.topLet' replaced
topLet :: Transformation
topLet = anyConstructor "topLet" 'Letrec.topLetWorker

-- | The current 'Clash.Normalize.Transformations.Letrec.simpleCSE', which
-- 'Clash.Normalize.Scoped.Transformations.Letrec.simpleCSE' replaced
simpleCSE :: Transformation
simpleCSE = toTransformation "CSE" (onLet 'Letrec.simpleCSEWorker)

-- | The current 'Clash.Normalize.Transformations.Letrec.recToLetRec', which
-- 'Clash.Normalize.Scoped.Transformations.Letrec.recToLetRec' replaced in the
-- normalization strategy
recToLetRec :: NormRewrite
recToLetRec = Letrec.recToLetRec

-- | The current 'Clash.Normalize.Transformations.Inline.bindConstantVar'
bindConstantVar :: Transformation
bindConstantVar = toTransformation "bindConstantVar" (onLet 'Inline.bindConstantVarWorker)

-- | The current 'Clash.Normalize.Transformations.Inline.inlineCast'
inlineCast :: Transformation
inlineCast = toTransformation "inlineCast" (onLet 'Inline.inlineCastWorker)

-- | The current 'Clash.Normalize.Transformations.Inline.inlineCleanup'
inlineCleanup :: Transformation
inlineCleanup = toTransformation "inlineCleanup" (onLet 'Inline.inlineCleanupWorker)

-- | The current 'Clash.Normalize.Transformations.Inline.collapseRHSNoops'
collapseRHSNoops :: Transformation
collapseRHSNoops = toTransformation "collapseRHSNoops" (onLet 'Inline.collapseRHSNoopsWorker)

-- | The current 'Clash.Normalize.Transformations.Inline.inlineOrLiftNonRep'
inlineOrLiftNonRep :: Transformation
inlineOrLiftNonRep =
  toTransformation "bindOrLiftNonRep" (onLet 'Inline.inlineOrLiftNonRepWorker)

-- | The current 'Clash.Normalize.Transformations.Inline.inlineSimIO'
inlineSimIO :: Transformation
inlineSimIO = toTransformation "bindSimIO" (onLet 'Inline.inlineSimIOWorker)

-- | The current 'Clash.Normalize.Transformations.Inline.inlineSmall'
inlineSmall :: Transformation
inlineSmall =
  toTransformation "inlineSmall"
    (onVarNode 'Inline.inlineSmallWorker <> onAppNode 'Inline.inlineSmallWorker
      <> onTyAppNode 'Inline.inlineSmallWorker <> onTickNode 'Inline.inlineSmallWorker)

-- | The current 'Clash.Normalize.Transformations.Inline.inlineWorkFree'
inlineWorkFree :: Transformation
inlineWorkFree =
  toTransformation "inlineWorkFree"
    (onVarNode 'Inline.inlineWorkFreeWorker <> onAppNode 'Inline.inlineWorkFreeWorker
      <> onTyAppNode 'Inline.inlineWorkFreeWorker <> onTickNode 'Inline.inlineWorkFreeWorker)

-- | The current 'Clash.Normalize.Transformations.Inline.inlineNonRep', a
-- whole-term pass, which the strategy replaced by
-- 'Clash.Normalize.Scoped.Transformations.Inline.inlineNonRep'
inlineNonRep :: NormRewrite
inlineNonRep = Inline.inlineNonRep

-- | The current 'Clash.Normalize.Transformations.Specialize.appProp', which
-- 'Clash.Normalize.Scoped.Transformations.Specialize.appProp' replaced
appProp :: Transformation
appProp = toTransformation "applicationPropagation"
  (onAppNode 'Specialize.appPropWorker <> onTyAppNode 'Specialize.appPropWorker)

-- | The current 'Clash.Normalize.Transformations.Specialize.constantSpec',
-- which 'Clash.Normalize.Scoped.Transformations.Specialize.constantSpec'
-- replaced
constantSpec :: Transformation
constantSpec = toTransformation "constantSpec" (onApp 'Specialize.constantSpecWorker)

-- | The current 'Clash.Normalize.Transformations.Specialize.nonRepSpec',
-- which 'Clash.Normalize.Scoped.Transformations.Specialize.nonRepSpec'
-- replaced
nonRepSpec :: Transformation
nonRepSpec = toTransformation "nonRepSpec" (onApp 'Specialize.nonRepSpecWorker)

-- | The current 'Clash.Normalize.Transformations.Specialize.typeSpec', which
-- 'Clash.Normalize.Scoped.Transformations.Specialize.typeSpec' replaced
typeSpec :: Transformation
typeSpec = toTransformation "typeSpec" (onTyApp 'Specialize.typeSpecWorker)

-- | The current 'Clash.Normalize.Transformations.Specialize.zeroWidthSpec',
-- which 'Clash.Normalize.Scoped.Transformations.Specialize.zeroWidthSpec'
-- replaced
zeroWidthSpec :: Transformation
zeroWidthSpec = toTransformation "zeroWidthSpec" (onLam 'Specialize.zeroWidthSpecWorker)

-- | The current 'Clash.Normalize.Transformations.Cast.argCastSpec', which
-- 'Clash.Normalize.Scoped.Transformations.Cast.argCastSpec' replaced
argCastSpec :: Transformation
argCastSpec = toTransformation "argCastSpec" (onApp 'Cast.argCastSpecWorker)
