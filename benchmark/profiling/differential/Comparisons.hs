{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The comparisons that @clash-differential@ can run. Register a comparison for
  a ported function by adding it to 'comparisons', see README.md.
-}

{-# LANGUAGE TemplateHaskell #-}

module Comparisons (comparisons) where

import qualified Clash.Normalize.Util as Current
import Clash.Rewrite.StrategyDSL.TH (compileStrategy)

import Clash.Core.Scoped.Convert (fromClashTermClosed, toClashTerm)
import Clash.Core.Scoped.Syntax (emptyScope)
import qualified Clash.Normalize.Scoped.Util as Scoped

import Test.Clash.Differential

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
