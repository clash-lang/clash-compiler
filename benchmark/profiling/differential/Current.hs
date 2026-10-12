{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The current implementations of the transformations that the normalizer runs
  ported versions of: those of "Clash.Normalize.Transformations", which the
  strategies no longer use (see "Clash.Normalize.Strategy.Spec"). They stay
  until Phase 7 of the port, to compare the ports with.
-}

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
  , bindConstantVar
  , inlineCast
  , inlineCleanup
  , collapseRHSNoops
  , inlineOrLiftNonRep
  , inlineSimIO
  , inlineSmall
  , inlineWorkFree
  , inlineNonRep
  , appProp
  , constantSpec
  , nonRepSpec
  , typeSpec
  , zeroWidthSpec
  , argCastSpec
  , caseCast
  , elimCastCast
  , letCast
  , splitCastWork
  , etaExpandSyn
  , etaExpansionTL
  , setupMultiResultPrim
  , separateArguments
  , xOptimize
  ) where

import Clash.Normalize.Transformations
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
  , bindConstantVar
  , inlineCast
  , inlineCleanup
  , collapseRHSNoops
  , inlineOrLiftNonRep
  , inlineSimIO
  , inlineSmall
  , inlineWorkFree
  , inlineNonRep
  , appProp
  , constantSpec
  , nonRepSpec
  , typeSpec
  , zeroWidthSpec
  , argCastSpec
  , caseCast
  , elimCastCast
  , letCast
  , splitCastWork
  , etaExpandSyn
  , etaExpansionTL
  , setupMultiResultPrim
  , separateArguments
  , xOptimize
  )
