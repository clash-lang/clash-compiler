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
  , reduceConst
  , reducePrim
  , removeUnusedExpr
  , simpleCSE
  , topLet
  ) where

import qualified Clash.Normalize.Transformations.ANF as ANF
import qualified Clash.Normalize.Transformations.Case as Case
import qualified Clash.Normalize.Transformations.DEC as DEC
import qualified Clash.Normalize.Transformations.Letrec as Letrec
import qualified Clash.Normalize.Transformations.Reduce as Reduce
import Clash.Rewrite.StrategyDSL
  ( Transformation, anyConstructor, onApp, onAppNode, onCase, onLet, onLetNode
  , onPrimNode, onTickNode
  , onTyAppNode, toTransformation )

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
