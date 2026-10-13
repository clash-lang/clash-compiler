{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.StrategyDSL.TH" compiling strategies of scoped
  transformations.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Rewrite.Scoped.StrategyDSL (tests) where

import qualified Data.List as List

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (..))

import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped (ScopedRewrite)
import qualified Clash.Rewrite.Scoped as S
import Clash.Rewrite.StrategyDSL
import Clash.Rewrite.StrategyDSL.TH

import Clash.Tests.Rewrite.Scoped.StrategyDSL.Stubs
import Test.Clash.Scoped (closedTerm, runScopedClosed)

-- | Assert that two scoped rewrites agree on a closed term: same result
-- (syntactically), same transformation count, same change flag
assertAgree :: ScopedRewrite () -> ScopedRewrite () -> Term 'VoidS -> Assertion
assertAgree candidate reference term = do
  (candidateTerm, candidateCount, candidateChanged) <- runScopedClosed candidate term
  (referenceTerm, referenceCount, referenceChanged) <- runScopedClosed reference term
  assertEqual "result" (show referenceTerm) (show candidateTerm)
  assertEqual "transformation count" referenceCount candidateCount
  assertEqual "change flag" referenceChanged candidateChanged

calleeFast :: ScopedRewrite ()
calleeFast = $(compileScopedStrategy (topdown literalStep))

calleeReference :: ScopedRewrite ()
calleeReference = $(compileScopedStrategyReference (topdown literalStep))

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Scoped.StrategyDSL"
  [ testGroup "singleton dispatch equals apply"
      [ testCase "Let transformation fires" $
          assertAgree
            $(scopedDispatchQ (one inlineTrivialLet))
            (S.apply "inlineTrivialLet" inlineTrivialLetRewrite)
            nestedLet
      , testCase "Let transformation offered a non-Let node" $
          assertAgree
            $(scopedDispatchQ (one inlineTrivialLet))
            (S.apply "inlineTrivialLet" inlineTrivialLetRewrite)
            simpleApplication
      , testCase "App transformation fires" $
          assertAgree
            $(scopedDispatchQ (one collapseApplication))
            (S.apply "collapseApplication" collapseApplicationRewrite)
            simpleApplication
      , testCase "multi-constructor transformation, both constructors" $ do
          assertAgree
            $(scopedDispatchQ (one incrementBoth))
            (S.apply "incrementBoth" incrementBothRewrite)
            simpleApplication
          assertAgree
            $(scopedDispatchQ (one incrementBoth))
            (S.apply "incrementBoth" incrementBothRewrite)
            (integer 3)
      ]

  , testGroup "bucket chain semantics"
      [ testCase "guarded suffix: later same-bucket member fires on changed constructor" $ do
          (result, count, changedFlag) <- runScopedClosed
            $(scopedDispatchQ (chain [collapseApplication, incrementBoth]))
            simpleApplication
          assertEqual "result" (show (integer 4 :: Term 'VoidS)) (show result)
          assertEqual "count" 2 count
          assertEqual "changed" True changedFlag
      , testCase "guarded suffix includes members of other buckets" $ do
          (result, count, _) <- runScopedClosed
            $(scopedDispatchQ (chain [collapseApplication, incrementOdd]))
            simpleApplication
          assertEqual "result" (show (integer 4 :: Term 'VoidS)) (show result)
          assertEqual "count" 2 count
      , testCase "no members for the node's constructor: untouched, no change" $ do
          (result, count, changedFlag) <- runScopedClosed
            $(scopedDispatchQ (one inlineTrivialLet))
            simpleApplication
          assertEqual "result" (show simpleApplication) (show result)
          assertEqual "count" 0 count
          assertEqual "changed" False changedFlag
      , testCase "anyConstructor member joins every bucket" $ do
          (result, count, _) <- runScopedClosed
            $(scopedDispatchQ (chain [collapseApplication, incrementOddAny]))
            simpleApplication
          assertEqual "result" (show (integer 4 :: Term 'VoidS)) (show result)
          assertEqual "count" 2 count
      ]

  , testGroup "compiled strategies equal their references" $
      let
        scenarios =
          [ ( "constructor-changing chain on nested term"
            , $(allTraversals constructorChangingStep)
            , nestedTerm
            )
          , ( "literal chain on nested term"
            , $(allTraversals literalStep)
            , nestedTerm
            )
          , ( "context probe on all constructors"
            , $(allTraversals contextProbeStep)
            , allConstructorsTerm
            )
          , ( "AppArg context tracks the rewritten function"
            , $(allTraversals appArgProbeStep)
            , appArgProbeTerm
            )
          , ( "no-op chain on all constructors"
            , $(allTraversals noOpStep)
            , allConstructorsTerm
            )
          , ( "anyConstructor member"
            , $(allTraversals anyWorkerStep)
            , nestedTerm
            )
          , ( "node-worker member"
            , $(allTraversals nodeWorkerStep)
            , nestedTerm
            )
          , ( "mid-traversal conditional"
            , $(allTraversals conditionalStep)
            , nestedTerm
            )
          , ( "embedded nested traversal"
            , $(allTraversals nestedTraversalStep)
            , nestedTerm
            )
          ]
      in
        [ testCase (scenarioName <> ": " <> traversalName) $
            assertAgree fast reference term
        | (scenarioName, traversals, term) <- scenarios
        , (traversalName, fast, reference) <- traversals
        ]

  , testGroup "compiled strategies equal hand-written compositions"
      [ testCase "topdown chain vs sequential apply chain" $
          assertAgree
            $(compileScopedStrategy (topdown constructorChangingStep))
            (S.topdownR
              (S.apply "collapseApplication" collapseApplicationRewrite S.>->
               S.apply "incrementBoth" incrementBothRewrite S.>->
               S.apply "inlineTrivialLet" inlineTrivialLetRewrite))
            nestedTerm
      ]

  , testGroup "strategy combinators"
      [ testCase "whole-term pass" $
          assertAgree
            $(compileScopedStrategy (scopedPass "incrementOddAny" 'incrementOddAnyWorker))
            $(compileScopedStrategyReference (scopedPass "incrementOddAny" 'incrementOddAnyWorker))
            nestedTerm
      , testCase "repeatR over sequenced traversals" $
          assertAgree
            $(compileScopedStrategy
                (repeatR (topdown collapseApplication >-> bottomup incrementOdd)))
            $(compileScopedStrategyReference
                (repeatR (topdown collapseApplication >-> bottomup incrementOdd)))
            nestedTerm
      , testCase "success and failure conditionals between strategies" $ do
          assertAgree
            $(compileScopedStrategy (topdown collapseApplication !-> bottomup incrementOdd))
            $(compileScopedStrategyReference
                (topdown collapseApplication !-> bottomup incrementOdd))
            nestedTerm
          assertAgree
            $(compileScopedStrategy (topdown inlineTrivialLet >-! bottomup incrementOdd))
            $(compileScopedStrategyReference
                (topdown inlineTrivialLet >-! bottomup incrementOdd))
            nestedTerm
      , testCase "deepseq between strategies" $
          assertAgree
            $(compileScopedStrategy (topdown collapseApplication >-!-> bottomup incrementOdd))
            $(compileScopedStrategyReference
                (topdown collapseApplication >-!-> bottomup incrementOdd))
            nestedTerm
      , testCase "calling another compiled strategy" $
          assertAgree
            $(compileScopedStrategy (callStrategyStrat 'calleeFast))
            $(compileScopedStrategyReference (callStrategyStrat 'calleeReference))
            nestedTerm
      ]

  , testGroup "descent"
      [ testCase "NonRec lets stay NonRec" $ do
          let term = closedTerm "let { !(x_0 :: T) = 3 } in 5"
          (result, _, _) <- runScopedClosed
            $(compileScopedStrategy (topdown literalStep)) term
          case result of
            Let (NonRec _ boundTerm) _body ->
              assertEqual "binder rewritten" (show (integer 4 :: Term 'VoidS)) (show boundTerm)
            _ -> assertFailure ("expected a NonRec let, got: " <> show result)
      , testCase "AppArg context in the probe result mentions the primitive" $ do
          (result, _, _) <- runScopedClosed
            $(compileScopedStrategy (topdown appArgProbeStep))
            appArgProbeTerm
          case result of
            Lam _ (App _function (Literal (StringLiteral recordedContext))) ->
              assertBool ("recorded context mentions somePrim: " <> recordedContext)
                ("somePrim" `List.isInfixOf` recordedContext)
            _ -> assertFailure ("expected a probed argument, got: " <> show result)
      ]

  , testGroup "asScopedRewriteQ"
      [ testCase "fires without instrumentation" $ do
          (result, count, changedFlag) <- runScopedClosed
            $(asScopedRewriteQ inlineTrivialLet) nestedLet
          assertEqual "result" (show (integer 5 :: Term 'VoidS)) (show result)
          assertEqual "count" 0 count
          assertEqual "changed" True changedFlag
      ]
  ]
 where
  nestedLet = closedTerm "let { !(x_0 :: T) = 5 } in x_0"
