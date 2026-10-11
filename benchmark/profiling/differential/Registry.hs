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

import Test.Clash.Differential (KnownDifference, coercionForm, letrecOrder, recToNonRec)

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
    [ (["deadCode", "deadcode"], $(asRewriteQ Current.deadCode), [letrecOrder])
    , (["DEC"], $(asRewriteQ Current.disjointExpressionConsolidation), [recToNonRec, letrecOrder])
    -- Case
    , (["caseCase"], $(asRewriteQ Current.caseCase), [])
    , (["caseCon"], $(asRewriteQ Current.caseCon), [coercionForm])
    , (["caseEliminateNonReachable"], $(asRewriteQ Current.caseEliminateNonReachable), [])
    , (["caseFlat"], $(asRewriteQ Current.caseFlat), [])
    , (["caseLet"], $(asRewriteQ Current.caseLet), [])
    , (["elimExistentials"], $(asRewriteQ Current.elimExistentials), [])
    , (["elimCaseBigNum"], $(asRewriteQ Current.elimCaseBigNumInternals), [])
    , (["nonRepANF"], $(asRewriteQ Current.nonRepANF), [])
    , (["ANF"], makeANF, [])
    , (["reduceConst"], $(asRewriteQ Current.reduceConst), [])
    , (["reducePrim"], $(asRewriteQ Current.reducePrim), [coercionForm])
    , (["removeUnusedExpr"], $(asRewriteQ Current.removeUnusedExpr), [coercionForm])
    , (["flattenLet", "letFlat"], $(asRewriteQ Current.flattenLet), [])
    , (["CSE"], $(asRewriteQ Current.simpleCSE), [letrecOrder])
    , (["topLet"], $(asRewriteQ Current.topLet), [])
    , (["recToLetRec"], Current.recToLetRec, [])
    ]

  unported =
    -- Cast
    [ (["argCastSpec"], $(asRewriteQ argCastSpec))
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
