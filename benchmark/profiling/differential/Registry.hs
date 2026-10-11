{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The current transformations of the normalizer, by the names under which
  they are captured (see "Clash.Rewrite.Capture"): their own names, the names
  that strategies give them ('Clash.Rewrite.StrategyDSL.named'), and the names
  of whole-term passes. @clash-differential --replay@ reruns every capture
  with its transformation of this list. Ported transformations are listed
  with their current implementation, see "Current".
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Registry
  ( currentRewrites
  , lookupCurrent
  ) where

import Data.Text (Text)

import Test.Clash.Differential (KnownDifference, letrecOrder)

import Clash.Normalize.Transformations
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)

import qualified Current

-- | The current transformations, by the names under which they are captured.
-- A ported transformation has the known differences with its port: the
-- normalizer runs the port, so its captured results are those of the port.
currentRewrites :: [([Text], NormRewrite, [KnownDifference])]
currentRewrites = map (\(nms, rw) -> (nms, rw, [])) unported <> ported
 where
  ported =
    [ (["deadCode", "deadcode"], $(asRewriteQ Current.deadCode), [letrecOrder]) ]

  unported =
    -- Case
    [ (["caseCase"], $(asRewriteQ caseCase))
    , (["caseCon"], $(asRewriteQ caseCon))
    , (["caseEliminateNonReachable"], $(asRewriteQ caseEliminateNonReachable))
    , (["caseFlat"], $(asRewriteQ caseFlat))
    , (["caseLet"], $(asRewriteQ caseLet))
    , (["elimExistentials"], $(asRewriteQ elimExistentials))
    , (["elimCaseBigNum"], $(asRewriteQ elimCaseBigNumInternals))
    -- ANF
    , (["nonRepANF"], $(asRewriteQ nonRepANF))
    , (["ANF"], makeANF)
    -- Letrec
    , (["removeUnusedExpr"], $(asRewriteQ removeUnusedExpr))
    , (["flattenLet", "letFlat"], $(asRewriteQ flattenLet))
    , (["CSE"], $(asRewriteQ simpleCSE))
    , (["topLet"], $(asRewriteQ topLet))
    , (["recToLetRec"], recToLetRec)
    -- DEC
    , (["DEC"], $(asRewriteQ disjointExpressionConsolidation))
    -- Cast
    , (["argCastSpec"], $(asRewriteQ argCastSpec))
    , (["caseCast"], $(asRewriteQ caseCast))
    , (["elimCastCast"], $(asRewriteQ elimCastCast))
    , (["letCast"], $(asRewriteQ letCast))
    , (["splitCastWork"], $(asRewriteQ splitCastWork))
    -- Specialize
    , (["applicationPropagation", "appProp", "appPropCS"], $(asRewriteQ appProp))
    , (["constantSpec"], $(asRewriteQ constantSpec))
    , (["nonRepSpec"], $(asRewriteQ nonRepSpec))
    , (["typeSpec"], $(asRewriteQ typeSpec))
    , (["zeroWidthSpec"], $(asRewriteQ zeroWidthSpec))
    -- Reduce
    , (["reduceConst"], $(asRewriteQ reduceConst))
    , (["reducePrim"], $(asRewriteQ reducePrim))
    -- Others
    , (["setupMultiResultPrim"], $(asRewriteQ setupMultiResultPrim))
    , (["xOptimize"], $(asRewriteQ xOptimize))
    , (["separateArguments"], $(asRewriteQ separateArguments))
    , (["etaExpandSyn"], $(asRewriteQ etaExpandSyn))
    , (["etaTL"], etaExpansionTL)
    -- Inline
    , (["bindConstantVar"], $(asRewriteQ bindConstantVar))
    , (["inlineCast"], $(asRewriteQ inlineCast))
    , (["inlineCleanup"], $(asRewriteQ inlineCleanup))
    , (["collapseRHSNoops"], $(asRewriteQ collapseRHSNoops))
    , (["bindOrLiftNonRep"], $(asRewriteQ inlineOrLiftNonRep))
    , (["bindSimIO"], $(asRewriteQ inlineSimIO))
    , (["inlineSmall"], $(asRewriteQ inlineSmall))
    , (["inlineWorkFree"], $(asRewriteQ inlineWorkFree))
    , (["inlineNonRep"], inlineNonRep)
    ]

-- | The current transformation captured under a name
lookupCurrent :: Text -> Maybe (NormRewrite, [KnownDifference])
lookupCurrent nm = case [(rw, kds) | (nms, rw, kds) <- currentRewrites, nm `elem` nms] of
  r : _ -> Just r
  [] -> Nothing
