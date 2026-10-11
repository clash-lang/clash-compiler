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
import qualified Clash.Normalize.Scoped.Transformations.DEC as S
import qualified Clash.Normalize.Scoped.Transformations.Letrec as S

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
  ]
