{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The comparisons that @clash-differential@ can run. Register a comparison for
  a ported function by adding it to 'comparisons', see README.md.
-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE OverloadedStrings #-}

module Comparisons (comparisons, siteComparisons) where

import Clash.Driver.Types (Binding (..))
import qualified Clash.Normalize.Util as Current
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ, asScopedRewriteQ, compileStrategy)
import qualified Clash.Normalize.Scoped.Transformations.ANF as S
import qualified Clash.Normalize.Scoped.Transformations.Case as SC
import qualified Clash.Normalize.Scoped.Transformations.Cast as S
import qualified Clash.Normalize.Scoped.Transformations.DEC as S
import qualified Clash.Normalize.Scoped.Transformations.EtaExpand as S
import qualified Clash.Normalize.Scoped.Transformations.Inline as SI
import qualified Clash.Normalize.Scoped.Transformations.Letrec as S
import qualified Clash.Normalize.Scoped.Transformations.MultiPrim as S
import qualified Clash.Normalize.Scoped.Transformations.Reduce as SReduce
import qualified Clash.Normalize.Scoped.Transformations.SeparateArgs as S
import qualified Clash.Normalize.Scoped.Transformations.Specialize as S
import qualified Clash.Normalize.Scoped.Transformations.XOptimize as S
import Clash.Normalize.Transformations.ANF (makeANF)
import Clash.Rewrite.Scoped (ScopedTransform (..))

import Clash.Core.Scoped.Convert (fromClashTermClosed, toClashTerm)
import Clash.Core.Scoped.Syntax (emptyScope, mkGlobalId)
import qualified Clash.Normalize.Scoped.Util as Scoped
import Clash.Rewrite.Scoped.ViaFoil (toClashTermFresh)

import Test.Clash.Differential
import Test.Clash.Differential.Sites

import qualified Current
import Demo

-- | All comparisons, by name
comparisons :: [Comparison]
comparisons =
  -- Conversion
  [ Comparison
      { cmpName = "roundtrip"
      , cmpDescription =
          "Identity: the conversion round trip, from current Core on the current side, \
          \from scoped Core on the scoped side"
      , cmpCurrent = pureSide (toClashTerm . fromClashTermClosed)
      , cmpScoped = pureSide (fromClashTermClosed . toClashTerm)
      , cmpKnownDifferences = []
      }

  -- Clash.Normalize.Scoped.Util
  , Comparison
      { cmpName = "substWithTyEq"
      , cmpDescription = "substWithTyEq, on every binding rather than only on top entities"
      , cmpCurrent = pureSide Current.substWithTyEq
      , cmpScoped = pureSide (Scoped.substWithTyEq emptyScope)
      , cmpKnownDifferences = []
      }
    -- Normalization depends on the uniques of the binders of its input, e.g.
    -- through the order of free variables, and on the uniques that the supply
    -- hands out. The scoped side converts its input to Clash Core with uniques
    -- from the supply ('viaLegacy'), so the current side does the same, to
    -- normalize the same input from the same supply.
  , Comparison
      { cmpName = "normalizeTopLvlBndr"
      , cmpDescription =
          "normalizeTopLvlBndr: normalize a binding. The scoped side runs the \
          \normalization strategy through viaLegacy."
      , cmpCurrent = normalizeSide $ \b tm -> do
          tm' <- toClashTermFresh (fromClashTermClosed tm)
          bindingTerm <$> Current.normalizeTopLvlBndr False (bindingId b) b{bindingTerm = tm'}
      , cmpScoped = normalizeSide $ \b tm ->
          bindingTerm <$> Scoped.normalizeTopLvlBndr False (mkGlobalId (bindingId b)) (tm <$ b)
      , cmpKnownDifferences = []
      }

  -- Demonstrations, see "Demo"
  , Comparison
      { cmpName = "demo-drop-ticks"
      , cmpDescription = "Broken on purpose: the scoped side drops all ticks"
      , cmpCurrent = pureSide id
      , cmpScoped = pureSide dropTicks
      , cmpKnownDifferences = []
      }
  , Comparison
      { cmpName = "demo-drop-ticks-known"
      , cmpDescription = "demo-drop-ticks, with dropping ticks as a known difference"
      , cmpCurrent = pureSide id
      , cmpScoped = pureSide dropTicks
      , cmpKnownDifferences = [ticksKnownDifference]
      }
  , Comparison
      { cmpName = "demo-caseOneAlt-once"
      , cmpDescription = "caseOneAlt, bottom-up once at every node"
      , cmpCurrent = currentRewrite "caseOneAlt" $(compileStrategy (strategyFor Once caseOneAltT))
      , cmpScoped = scopedRewrite Once caseOneAltS
      , cmpKnownDifferences = []
      }
  , Comparison
      { cmpName = "demo-caseOneAlt-fix"
      , cmpDescription = "caseOneAlt, to a fixpoint"
      , cmpCurrent = currentRewrite "caseOneAlt" $(compileStrategy (strategyFor Fixpoint caseOneAltT))
      , cmpScoped = scopedRewrite Fixpoint caseOneAltS
      , cmpKnownDifferences = []
      }
  , Comparison
      { cmpName = "demo-caseOneAlt-broken"
      , cmpDescription =
          "Broken on purpose: caseOneAlt to a fixpoint, keeping the first alternative \
          \that doesn't mention its pattern's variables"
      , cmpCurrent = currentRewrite "caseOneAlt" $(compileStrategy (strategyFor Fixpoint caseOneAltT))
      , cmpScoped = scopedRewrite Fixpoint caseOneAltBrokenS
      , cmpKnownDifferences = []
      }
  , Comparison
      { cmpName = "demo-caseOneAlt-fires"
      , cmpDescription =
          "caseOneAlt against the identity: the bindings that are different are \
          \those where it fires"
      , cmpCurrent = currentRewrite "caseOneAlt" $(compileStrategy (strategyFor Fixpoint caseOneAltT))
      , cmpScoped = pureSide id
      , cmpKnownDifferences = []
      }
  ]

-- | All comparisons at captured sites, by name, see "Test.Clash.Differential.Sites"
siteComparisons :: [SiteComparison]
siteComparisons =
  [ SiteComparison
      { scName = "deadCode"
      , scDescription =
          "The current deadCode (Clash.Normalize.Transformations.Letrec.deadCodeWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Letrec.deadCode)"
      , scTransformations = ["deadCode", "deadcode"]
      , scCurrent = $(asRewriteQ Current.deadCode)
      , scScoped = Just $(asScopedRewriteQ S.deadCode)
      , scKnownDifferences = [letrecOrder]
      }
  , SiteComparison
      { scName = "DEC"
      , scDescription =
          "The current disjointExpressionConsolidation \
          \(Clash.Normalize.Transformations.DEC.disjointExpressionConsolidationWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.DEC)"
      , scTransformations = ["DEC"]
      , scCurrent = $(asRewriteQ Current.disjointExpressionConsolidation)
      , scScoped = Just $(asScopedRewriteQ S.disjointExpressionConsolidation)
      , scKnownDifferences = [recToNonRec, letrecOrder]
      }

  -- Clash.Normalize.Scoped.Transformations.Case
  , SiteComparison
      { scName = "caseCase"
      , scDescription =
          "The current caseCase (Clash.Normalize.Transformations.Case.caseCaseWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Case.caseCase)"
      , scTransformations = ["caseCase"]
      , scCurrent = $(asRewriteQ Current.caseCase)
      , scScoped = Just $(asScopedRewriteQ SC.caseCase)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "caseCon"
      , scDescription =
          "The current caseCon (Clash.Normalize.Transformations.Case.caseConWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Case.caseCon)"
      , scTransformations = ["caseCon"]
      , scCurrent = $(asRewriteQ Current.caseCon)
      , scScoped = Just $(asScopedRewriteQ SC.caseCon)
      , scKnownDifferences = [coercionForm]
      }
  , SiteComparison
      { scName = "caseEliminateNonReachable"
      , scDescription =
          "The current caseEliminateNonReachable \
          \(Clash.Normalize.Transformations.Case.caseEliminateNonReachableWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Case.caseEliminateNonReachable)"
      , scTransformations = ["caseEliminateNonReachable"]
      , scCurrent = $(asRewriteQ Current.caseEliminateNonReachable)
      , scScoped = Just $(asScopedRewriteQ SC.caseEliminateNonReachable)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "caseFlat"
      , scDescription =
          "The current caseFlat (Clash.Normalize.Transformations.Case.caseFlatWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Case.caseFlat)"
      , scTransformations = ["caseFlat"]
      , scCurrent = $(asRewriteQ Current.caseFlat)
      , scScoped = Just $(asScopedRewriteQ SC.caseFlat)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "caseLet"
      , scDescription =
          "The current caseLet (Clash.Normalize.Transformations.Case.caseLetWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Case.caseLet)"
      , scTransformations = ["caseLet"]
      , scCurrent = $(asRewriteQ Current.caseLet)
      , scScoped = Just $(asScopedRewriteQ SC.caseLet)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "elimExistentials"
      , scDescription =
          "The current elimExistentials \
          \(Clash.Normalize.Transformations.Case.elimExistentialsWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Case.elimExistentials)"
      , scTransformations = ["elimExistentials"]
      , scCurrent = $(asRewriteQ Current.elimExistentials)
      , scScoped = Just $(asScopedRewriteQ SC.elimExistentials)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "elimCaseBigNum"
      , scDescription =
          "The current elimCaseBigNumInternals \
          \(Clash.Normalize.Transformations.Case.elimCaseBigNumInternalsWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Case.elimCaseBigNumInternals)"
      , scTransformations = ["elimCaseBigNum"]
      , scCurrent = $(asRewriteQ Current.elimCaseBigNumInternals)
      , scScoped = Just $(asScopedRewriteQ SC.elimCaseBigNumInternals)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "nonRepANF"
      , scDescription =
          "The current nonRepANF (Clash.Normalize.Transformations.ANF.nonRepANFWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.ANF.nonRepANF)"
      , scTransformations = ["nonRepANF"]
      , scCurrent = $(asRewriteQ Current.nonRepANF)
      , scScoped = Just $(asScopedRewriteQ S.nonRepANF)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "ANF"
      , scDescription =
          "The current makeANF (Clash.Normalize.Transformations.ANF.makeANF) \
          \against its port (Clash.Normalize.Scoped.Transformations.ANF.makeANF)"
      , scTransformations = ["ANF"]
      , scCurrent = makeANF
      , scScoped = Just (ScopedTransform S.makeANF)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "reduceConst"
      , scDescription =
          "The current reduceConst (Clash.Normalize.Transformations.Reduce.reduceConstWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Reduce.reduceConst)"
      , scTransformations = ["reduceConst"]
      , scCurrent = $(asRewriteQ Current.reduceConst)
      , scScoped = Just $(asScopedRewriteQ SReduce.reduceConst)
      , scKnownDifferences = [coercionForm]
      }
  , SiteComparison
      { scName = "reducePrim"
      , scDescription =
          "The current reducePrim (Clash.Normalize.Transformations.Reduce.reducePrimWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Reduce.reducePrim)"
      , scTransformations = ["reducePrim"]
      , scCurrent = $(asRewriteQ Current.reducePrim)
      , scScoped = Just $(asScopedRewriteQ SReduce.reducePrim)
      , scKnownDifferences = [coercionForm]
      }
  , SiteComparison
      { scName = "removeUnusedExpr"
      , scDescription =
          "The current removeUnusedExpr (Clash.Normalize.Transformations.Letrec) \
          \against its port (Clash.Normalize.Scoped.Transformations.Letrec.removeUnusedExpr)"
      , scTransformations = ["removeUnusedExpr"]
      , scCurrent = $(asRewriteQ Current.removeUnusedExpr)
      , scScoped = Just $(asScopedRewriteQ S.removeUnusedExpr)
      , scKnownDifferences = [coercionForm]
      }
  , SiteComparison
      { scName = "flattenLet"
      , scDescription =
          "The current flattenLet (Clash.Normalize.Transformations.Letrec.flattenLetWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Letrec.flattenLet)"
      , scTransformations = ["flattenLet", "letFlat"]
      , scCurrent = $(asRewriteQ Current.flattenLet)
      , scScoped = Just $(asScopedRewriteQ S.flattenLet)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "CSE"
      , scDescription =
          "The current simpleCSE (Clash.Normalize.Transformations.Letrec.simpleCSEWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Letrec.simpleCSE)"
      , scTransformations = ["CSE"]
      , scCurrent = $(asRewriteQ Current.simpleCSE)
      , scScoped = Just $(asScopedRewriteQ S.simpleCSE)
      , scKnownDifferences = [letrecOrder]
      }
  , SiteComparison
      { scName = "topLet"
      , scDescription =
          "The current topLet (Clash.Normalize.Transformations.Letrec.topLetWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Letrec.topLet)"
      , scTransformations = ["topLet"]
      , scCurrent = $(asRewriteQ Current.topLet)
      , scScoped = Just $(asScopedRewriteQ S.topLet)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "recToLetRec"
      , scDescription =
          "The current recToLetRec (Clash.Normalize.Transformations.Letrec.recToLetRec) \
          \against its port (Clash.Normalize.Scoped.Transformations.Letrec.recToLetRec)"
      , scTransformations = ["recToLetRec"]
      , scCurrent = Current.recToLetRec
      , scScoped = Just (ScopedTransform S.recToLetRec)
      , scKnownDifferences = []
      }

  -- Clash.Normalize.Scoped.Transformations.Inline
  , inlineSites "bindConstantVar" ["bindConstantVar"]
      $(asRewriteQ Current.bindConstantVar) $(asScopedRewriteQ SI.bindConstantVar)
  , inlineSites "inlineCast" ["inlineCast"]
      $(asRewriteQ Current.inlineCast) $(asScopedRewriteQ SI.inlineCast)
  , inlineSites "inlineCleanup" ["inlineCleanup"]
      $(asRewriteQ Current.inlineCleanup) $(asScopedRewriteQ SI.inlineCleanup)
  , inlineSites "collapseRHSNoops" ["collapseRHSNoops"]
      $(asRewriteQ Current.collapseRHSNoops) $(asScopedRewriteQ SI.collapseRHSNoops)
  , inlineSites "inlineOrLiftNonRep" ["bindOrLiftNonRep"]
      $(asRewriteQ Current.inlineOrLiftNonRep) $(asScopedRewriteQ SI.inlineOrLiftNonRep)
  , inlineSites "inlineSimIO" ["bindSimIO"]
      $(asRewriteQ Current.inlineSimIO) $(asScopedRewriteQ SI.inlineSimIO)
  , inlineSites "inlineSmall" ["inlineSmall"]
      $(asRewriteQ Current.inlineSmall) $(asScopedRewriteQ SI.inlineSmall)
  , inlineSites "inlineWorkFree" ["inlineWorkFree"]
      $(asRewriteQ Current.inlineWorkFree) $(asScopedRewriteQ SI.inlineWorkFree)
  , inlineSites "inlineNonRep" ["inlineNonRep"]
      Current.inlineNonRep (ScopedTransform SI.inlineNonRep)

  -- Specialize
  , SiteComparison
      { scName = "appProp"
      , scDescription =
          "The current appProp (Clash.Normalize.Transformations.Specialize.appPropWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Specialize.appProp)"
      , scTransformations = ["applicationPropagation", "appProp", "appPropCS"]
      , scCurrent = $(asRewriteQ Current.appProp)
      , scScoped = Just $(asScopedRewriteQ S.appProp)
      , scKnownDifferences = [nonRecLetOrder]
      }
  , SiteComparison
      { scName = "constantSpec"
      , scDescription =
          "The current constantSpec (Clash.Normalize.Transformations.Specialize.constantSpecWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Specialize.constantSpec)"
      , scTransformations = ["constantSpec"]
      , scCurrent = $(asRewriteQ Current.constantSpec)
      , scScoped = Just $(asScopedRewriteQ S.constantSpec)
      , scKnownDifferences = [nonRecLetOrder]
      }
  , SiteComparison
      { scName = "nonRepSpec"
      , scDescription =
          "The current nonRepSpec (Clash.Normalize.Transformations.Specialize.nonRepSpecWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Specialize.nonRepSpec)"
      , scTransformations = ["nonRepSpec"]
      , scCurrent = $(asRewriteQ Current.nonRepSpec)
      , scScoped = Just $(asScopedRewriteQ S.nonRepSpec)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "typeSpec"
      , scDescription =
          "The current typeSpec (Clash.Normalize.Transformations.Specialize.typeSpecWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Specialize.typeSpec)"
      , scTransformations = ["typeSpec"]
      , scCurrent = $(asRewriteQ Current.typeSpec)
      , scScoped = Just $(asScopedRewriteQ S.typeSpec)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "zeroWidthSpec"
      , scDescription =
          "The current zeroWidthSpec (Clash.Normalize.Transformations.Specialize.zeroWidthSpecWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Specialize.zeroWidthSpec)"
      , scTransformations = ["zeroWidthSpec"]
      , scCurrent = $(asRewriteQ Current.zeroWidthSpec)
      , scScoped = Just $(asScopedRewriteQ S.zeroWidthSpec)
      , scKnownDifferences = []
      }

  -- Cast
  , SiteComparison
      { scName = "argCastSpec"
      , scDescription =
          "The current argCastSpec (Clash.Normalize.Transformations.Cast.argCastSpecWorker) \
          \against its port (Clash.Normalize.Scoped.Transformations.Cast.argCastSpec)"
      , scTransformations = ["argCastSpec"]
      , scCurrent = $(asRewriteQ Current.argCastSpec)
      , scScoped = Just $(asScopedRewriteQ S.argCastSpec)
      , scKnownDifferences = []
      }
    -- Clash.Normalize.Transformations.Cast
  , SiteComparison
      { scName = "caseCast"
      , scDescription = "The current caseCast against its port"
      , scTransformations = ["caseCast"]
      , scCurrent = $(asRewriteQ Current.caseCast)
      , scScoped = Just $(asScopedRewriteQ S.caseCast)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "elimCastCast"
      , scDescription = "The current elimCastCast against its port"
      , scTransformations = ["elimCastCast"]
      , scCurrent = $(asRewriteQ Current.elimCastCast)
      , scScoped = Just $(asScopedRewriteQ S.elimCastCast)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "letCast"
      , scDescription = "The current letCast against its port"
      , scTransformations = ["letCast"]
      , scCurrent = $(asRewriteQ Current.letCast)
      , scScoped = Just $(asScopedRewriteQ S.letCast)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "splitCastWork"
      , scDescription = "The current splitCastWork against its port"
      , scTransformations = ["splitCastWork"]
      , scCurrent = $(asRewriteQ Current.splitCastWork)
      , scScoped = Just $(asScopedRewriteQ S.splitCastWork)
      , scKnownDifferences = []
      }
  -- Clash.Normalize.Transformations.EtaExpand
  , SiteComparison
      { scName = "etaExpandSyn"
      , scDescription = "The current etaExpandSyn against its port"
      , scTransformations = ["etaExpandSyn"]
      , scCurrent = $(asRewriteQ Current.etaExpandSyn)
      , scScoped = Just $(asScopedRewriteQ S.etaExpandSyn)
      , scKnownDifferences = []
      }
  , SiteComparison
      { scName = "etaTL"
      , scDescription =
          "The current etaExpansionTL (Clash.Normalize.Transformations.EtaExpand) \
          \against its port (Clash.Normalize.Scoped.Transformations.EtaExpand)"
      , scTransformations = ["etaTL"]
      , scCurrent = Current.etaExpansionTL
      , scScoped = Just (ScopedTransform S.etaExpansionTL)
      , scKnownDifferences = []
      }
  -- Clash.Normalize.Transformations.MultiPrim
  , SiteComparison
      { scName = "setupMultiResultPrim"
      , scDescription = "The current setupMultiResultPrim against its port"
      , scTransformations = ["setupMultiResultPrim"]
      , scCurrent = $(asRewriteQ Current.setupMultiResultPrim)
      , scScoped = Just $(asScopedRewriteQ S.setupMultiResultPrim)
      , scKnownDifferences = [letrecOrder]
      }
  -- Clash.Normalize.Transformations.SeparateArgs
  , SiteComparison
      { scName = "separateArguments"
      , scDescription = "The current separateArguments against its port"
      , scTransformations = ["separateArguments"]
      , scCurrent = $(asRewriteQ Current.separateArguments)
      , scScoped = Just $(asScopedRewriteQ S.separateArguments)
      , scKnownDifferences = [coercionForm]
      }
  -- Clash.Normalize.Transformations.XOptimize
  , SiteComparison
      { scName = "xOptimize"
      , scDescription = "The current xOptimize against its port"
      , scTransformations = ["xOptimize"]
      , scCurrent = $(asRewriteQ Current.xOptimize)
      , scScoped = Just $(asScopedRewriteQ S.xOptimize)
      , scKnownDifferences = [primTypeForm]
      }
  ]

 where
  inlineSites nm capNames cur scoped = SiteComparison
    { scName = nm
    , scDescription =
        "The current " <> nm <> " (Clash.Normalize.Transformations.Inline) against its port \
        \(Clash.Normalize.Scoped.Transformations.Inline)"
    , scTransformations = capNames
    , scCurrent = cur
    , scScoped = Just scoped
    , scKnownDifferences = [letrecOrder, globalUniques]
    }
