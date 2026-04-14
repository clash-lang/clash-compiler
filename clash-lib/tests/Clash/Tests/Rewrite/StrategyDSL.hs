{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.StrategyDSL.TH": the chain semantics of compiled
  dispatch (bucket order, the guarded suffix after a mid-chain constructor
  change) and the equivalence of every compiled strategy with its unfused
  reference (both generated from the same spec) and with hand-written
  'Clash.Rewrite.Combinators' compositions.
-}

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

--{-# OPTIONS_GHC -ddump-splices #-}

module Clash.Tests.Rewrite.StrategyDSL (tests) where

import Control.Concurrent.MVar (readMVar)
import Data.Default (def)

import Clash.Core.Literal (Literal (..))
import Clash.Core.Term (Bind (..), Term (..))
import Clash.Core.VarEnv (emptyInScopeSet)
import Clash.Rewrite.StrategyDSL
import Clash.Rewrite.StrategyDSL.TH
import Clash.Rewrite.Types (Rewrite, RewriteState (..), TransformContext (..), runR)
import Clash.Rewrite.Util (apply)
import Clash.Tests.Rewrite.StrategyDSL.Stubs
import Test.Clash.Rewrite (defRewriteState)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Rewrite.Combinators as Comb
import qualified Data.List as List
import qualified Data.Monoid as Monoid

-- * Running rewrites in a default environment

emptyContext :: TransformContext
emptyContext = TransformContext emptyInScopeSet []

-- | Run a rewrite on a term in the default (silent) environment, returning
-- the result term, the transformation counter, and the change flag.
runRewriteOn :: Rewrite () -> Term -> IO (Term, Word, Bool)
runRewriteOn rewrite term = do
  st <- defRewriteState ()
  (result, state, anyChanged) <- runR (rewrite emptyContext term) def st
  -- The total number of applied transformations is the sum of the per-
  -- transformation counters.
  nTrans <- sum <$> readMVar (_transformAppliedCounters state)
  pure (result, nTrans, Monoid.getAny anyChanged)

-- | Assert that two rewrites agree on a term: same result (syntactically),
-- same transformation count, same change flag.
assertAgree :: Rewrite () -> Rewrite () -> Term -> Assertion
assertAgree candidate reference term = do
  (candidateTerm, candidateCount, candidateChanged) <- runRewriteOn candidate term
  (referenceTerm, referenceCount, referenceChanged) <- runRewriteOn reference term
  assertEqual "result" (show referenceTerm) (show candidateTerm)
  assertEqual "transformation count" referenceCount candidateCount
  assertEqual "change flag" referenceChanged candidateChanged

-- * Compiled strategies referenced by other compiled strategies

calleeFast :: Rewrite ()
calleeFast = $(compileStrategy (topdown literalStep))

calleeReference :: Rewrite ()
calleeReference = $(compileStrategyReference (topdown literalStep))

-- * Tests

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.StrategyDSL"
  [ testGroup "singleton dispatch equals apply"
      [ testCase "Let transformation fires" $
          assertAgree
            $(dispatchQ (one inlineTrivialLet))
            (apply "inlineTrivialLet" inlineTrivialLetRewrite)
            (Let (NonRec (someId 0) (integer 5)) (Var (someId 0)))
      , testCase "Let transformation offered a non-Let node" $
          assertAgree
            $(dispatchQ (one inlineTrivialLet))
            (apply "inlineTrivialLet" inlineTrivialLetRewrite)
            simpleApplication
      , testCase "App transformation fires" $
          assertAgree
            $(dispatchQ (one collapseApplication))
            (apply "collapseApplication" collapseApplicationRewrite)
            simpleApplication
      , testCase "multi-constructor transformation, both constructors" $ do
          assertAgree
            $(dispatchQ (one incrementBoth))
            (apply "incrementBoth" incrementBothRewrite)
            simpleApplication
          assertAgree
            $(dispatchQ (one incrementBoth))
            (apply "incrementBoth" incrementBothRewrite)
            (integer 3)
      ]

  , testGroup "bucket chain semantics"
      [ testCase
          "guarded suffix: later same-bucket member fires on changed constructor"
          $ do
          -- collapseApplication turns @f 3@ into @3@; incrementBoth is later
          -- in the App bucket and must still fire on the literal, through its
          -- own Literal worker.
          (result, count, changedFlag) <- runRewriteOn
            $(dispatchQ (chain [collapseApplication, incrementBoth]))
            simpleApplication
          assertEqual "result" (show (integer 4)) (show result)
          assertEqual "count" 2 count
          assertEqual "changed" True changedFlag
      , testCase "guarded suffix includes members of other buckets" $ do
          -- incrementOdd is registered for Literal only, so it is not in the
          -- App bucket. It must still be offered the literal that
          -- collapseApplication left behind, like a plain apply chain would.
          (result, count, _) <- runRewriteOn
            $(dispatchQ (chain [collapseApplication, incrementOdd]))
            simpleApplication
          assertEqual "result" (show (integer 4)) (show result)
          assertEqual "count" 2 count
      , testCase "earlier members do not rerun after a change" $ do
          -- incrementBoth (App worker) fires first, turning @f 3@ into @f 4@;
          -- collapseApplication is later in the App bucket and collapses to
          -- @4@. incrementBoth must not see the literal again.
          (result, count, _) <- runRewriteOn
            $(dispatchQ (chain [incrementBoth, collapseApplication]))
            simpleApplication
          assertEqual "result" (show (integer 4)) (show result)
          assertEqual "count" 2 count
      , testCase "no members for the node's constructor: untouched, no change" $ do
          (result, count, changedFlag) <- runRewriteOn
            $(dispatchQ (one inlineTrivialLet))
            simpleApplication
          assertEqual "result" (show simpleApplication) (show result)
          assertEqual "count" 0 count
          assertEqual "changed" False changedFlag
      , testCase "anyConstructor member joins every bucket" $ do
          -- collapseApplication turns @f 3@ into @3@; incrementOddAny is
          -- constructor-agnostic, so it is in the App bucket too and fires on the
          -- literal through its own match.
          (result, count, _) <- runRewriteOn
            $(dispatchQ (chain [collapseApplication, incrementOddAny]))
            simpleApplication
          assertEqual "result" (show (integer 4)) (show result)
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
            $(compileStrategy (topdown constructorChangingStep))
            (Comb.topdownR
              (apply "collapseApplication" collapseApplicationRewrite Comb.>->
               apply "incrementBoth" incrementBothRewrite Comb.>->
               apply "inlineTrivialLet" inlineTrivialLetRewrite))
            nestedTerm
      , testCase "topdown anyConstructor vs plain rewrite" $
          assertAgree
            $(compileStrategy (topdown incrementOddAny))
            (Comb.topdownR (apply "incrementOddAny" incrementOddAnyWorker))
            nestedTerm
      ]

  , testGroup "strategy combinators"
      [ testCase "whole-term pass" $
          assertAgree
            $(compileStrategy (pass "incrementOddAny" 'incrementOddAnyWorker))
            $(compileStrategyReference (pass "incrementOddAny" 'incrementOddAnyWorker))
            nestedTerm
      , testCase "repeatR over sequenced traversals" $
          assertAgree
            $(compileStrategy
                (repeatR (topdown collapseApplication >-> bottomup incrementOdd)))
            $(compileStrategyReference
                (repeatR (topdown collapseApplication >-> bottomup incrementOdd)))
            nestedTerm
      , testCase "success and failure conditionals between strategies" $ do
          assertAgree
            $(compileStrategy (topdown collapseApplication !-> bottomup incrementOdd))
            $(compileStrategyReference
                (topdown collapseApplication !-> bottomup incrementOdd))
            nestedTerm
          assertAgree
            $(compileStrategy (topdown inlineTrivialLet >-! bottomup incrementOdd))
            $(compileStrategyReference
                (topdown inlineTrivialLet >-! bottomup incrementOdd))
            nestedTerm
      , testCase "deepseq between strategies" $
          assertAgree
            $(compileStrategy (topdown collapseApplication >-!-> bottomup incrementOdd))
            $(compileStrategyReference
                (topdown collapseApplication >-!-> bottomup incrementOdd))
            nestedTerm
      , testCase "calling another compiled strategy" $
          assertAgree
            $(compileStrategy (callStrategyStrat 'calleeFast))
            $(compileStrategyReference (callStrategyStrat 'calleeReference))
            nestedTerm
      ]

  , testGroup "descent"
      [ testCase "NonRec lets are rebuilt as Rec, like allR" $ do
          -- See Note [NonRec erasure during descent] in
          -- Clash.Rewrite.StrategyDSL.TH.
          let term = Let (NonRec (someId 0) (integer 3)) (integer 5)
          (result, _, _) <- runRewriteOn
            $(compileStrategy (topdown literalStep)) term
          case result of
            Let (Rec [(_, boundTerm)]) _body ->
              assertEqual "binder rewritten" (show (integer 4)) (show boundTerm)
            _ -> assertFailure ("expected a Rec let, got: " <> show result)
      , testCase "AppArg context in the probe result mentions the primitive" $ do
          -- The argument is probed after the function position rewrote to a
          -- primitive; its recorded context must carry that primitive's
          -- 'primArg' information, proving the argument context is computed
          -- from the rewritten function.
          (result, _, _) <- runRewriteOn
            $(compileStrategy (topdown appArgProbeStep))
            appArgProbeTerm
          case result of
            App _function (Literal (StringLiteral recordedContext)) ->
              assertBool ("recorded context mentions somePrim: " <> recordedContext)
                ("somePrim" `List.isInfixOf` recordedContext)
            _ -> assertFailure ("expected a probed argument, got: " <> show result)
      ]

  , testGroup "asRewriteQ"
      [ testCase "fires without instrumentation" $ do
          (result, count, changedFlag) <- runRewriteOn
            $(asRewriteQ inlineTrivialLet)
            (Let (NonRec (someId 0) (integer 5)) (Var (someId 0)))
          assertEqual "result" (show (integer 5)) (show result)
          assertEqual "count" 0 count
          assertEqual "changed" True changedFlag
      , testCase "non-matching node untouched" $ do
          (result, count, changedFlag) <- runRewriteOn
            $(asRewriteQ inlineTrivialLet)
            simpleApplication
          assertEqual "result" (show simpleApplication) (show result)
          assertEqual "count" 0 count
          assertEqual "changed" False changedFlag
      ]
  ]
