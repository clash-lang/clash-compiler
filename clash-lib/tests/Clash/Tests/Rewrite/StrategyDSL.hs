{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.StrategyDSL.Compile": the chain semantics of
  compiled dispatch (bucket order, the guarded suffix after a mid-chain
  constructor change) and the equivalence of every compiled strategy with
  its unfused reference — both compiled from the same spec — and with
  hand-written 'Clash.Rewrite.Combinators' compositions.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Rewrite.StrategyDSL (tests) where

import qualified Data.List as List
import qualified Data.Monoid as Monoid
import Data.Default (def)

import Clash.Core.Literal (Literal (..))
import Clash.Core.Term (Bind (..), Term (..))
import Clash.Core.VarEnv (emptyInScopeSet)
import Clash.Normalize.Strategy (constantPropagation)
import Clash.Normalize.Strategy.Spec
  (constantPropagationSpec, flattenSpec, normalizationSpec)
import qualified Clash.Rewrite.Combinators as Comb
import Clash.Rewrite.StrategyDSL
import Clash.Rewrite.StrategyDSL.Compile
import Clash.Rewrite.Types (Rewrite, RewriteState (..), TransformContext (..), runR)
import Clash.Rewrite.Util (apply)
import Clash.Tests.Rewrite.StrategyDSL.Stubs
import Test.Clash.Rewrite ()

import Test.Tasty
import Test.Tasty.HUnit

-- * Running rewrites in a default environment

emptyContext :: TransformContext
emptyContext = TransformContext emptyInScopeSet []

-- | Run a rewrite on a term in the default (silent) environment, returning
-- the result term, the transformation counter, and the change flag.
runRewriteOn :: Rewrite () -> Term -> IO (Term, Word, Bool)
runRewriteOn rewrite term = do
  (result, state, anyChanged) <- runR (rewrite emptyContext term) def def
  pure (result, _transformCounter state, Monoid.getAny anyChanged)

-- | Assert that two rewrites agree on a term: same result (syntactically),
-- same transformation count, same change flag.
assertAgree :: String -> Rewrite () -> Rewrite () -> Term -> Assertion
assertAgree label candidate reference term = do
  (candidateTerm, candidateCount, candidateChanged) <- runRewriteOn candidate term
  (referenceTerm, referenceCount, referenceChanged) <- runRewriteOn reference term
  assertEqual (label <> ": result") (show referenceTerm) (show candidateTerm)
  assertEqual (label <> ": transformation count") referenceCount candidateCount
  assertEqual (label <> ": change flag") referenceChanged candidateChanged

-- * Compiled strategies referenced by other compiled strategies

calleeFast :: Rewrite ()
calleeFast = compile (TopDown literalStep)

calleeReference :: Rewrite ()
calleeReference = compileReference (TopDown literalStep)

-- * Tests

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.StrategyDSL"
  [ testGroup "singleton dispatch equals apply"
      [ testCase "Let transformation fires" $
          assertAgree "inlineTrivialLet"
            (dispatch (one inlineTrivialLet))
            (apply "inlineTrivialLet" inlineTrivialLetRewrite)
            (Let (NonRec (someId 0) (integer 5)) (Var (someId 0)))
      , testCase "Let transformation offered a non-Let node" $
          assertAgree "inlineTrivialLet"
            (dispatch (one inlineTrivialLet))
            (apply "inlineTrivialLet" inlineTrivialLetRewrite)
            simpleApplication
      , testCase "App transformation fires" $
          assertAgree "collapseApplication"
            (dispatch (one collapseApplication))
            (apply "collapseApplication" collapseApplicationRewrite)
            simpleApplication
      , testCase "multi-shape transformation, both shapes" $ do
          assertAgree "incrementBoth on App"
            (dispatch (one incrementBoth))
            (apply "incrementBoth" incrementBothRewrite)
            simpleApplication
          assertAgree "incrementBoth on Literal"
            (dispatch (one incrementBoth))
            (apply "incrementBoth" incrementBothRewrite)
            (integer 3)
      ]

  , testGroup "bucket chain semantics"
      [ testCase "guarded suffix: later same-bucket member fires on changed shape" $ do
          -- collapseApplication turns @f 3@ into @3@; incrementBoth is later
          -- in the App bucket and must still fire — on the literal, through
          -- its own Literal entry.
          (result, count, changedFlag) <- runRewriteOn
            (dispatch (chain [collapseApplication, incrementBoth]))
            simpleApplication
          assertEqual "result" (show (integer 4)) (show result)
          assertEqual "count" 2 count
          assertEqual "changed" True changedFlag
      , testCase "members of other buckets do not run in the same pass" $ do
          -- incrementOdd is registered for Literal only; after
          -- collapseApplication changes App to Literal it must NOT run in
          -- this dispatch pass — it was never in the App bucket.
          (result, count, _) <- runRewriteOn
            (dispatch (chain [collapseApplication, incrementOdd]))
            simpleApplication
          assertEqual "result" (show (integer 3)) (show result)
          assertEqual "count" 1 count
      , testCase "earlier members do not rerun after a change" $ do
          -- incrementBoth (App entry) fires first, turning @f 3@ into @f 4@;
          -- collapseApplication is later in the App bucket and collapses to
          -- @4@. incrementBoth must not see the literal again.
          (result, count, _) <- runRewriteOn
            (dispatch (chain [incrementBoth, collapseApplication]))
            simpleApplication
          assertEqual "result" (show (integer 4)) (show result)
          assertEqual "count" 2 count
      , testCase "no members for the node's constructor: untouched, no change" $ do
          (result, count, changedFlag) <- runRewriteOn
            (dispatch (one inlineTrivialLet))
            simpleApplication
          assertEqual "result" (show simpleApplication) (show result)
          assertEqual "count" 0 count
          assertEqual "changed" False changedFlag
      , testCase "anyShape member joins every bucket" $ do
          -- collapseApplication turns @f 3@ into @3@; incrementOddAny is
          -- shape-agnostic, so it is in the App bucket too and fires on the
          -- literal through its own match.
          (result, count, _) <- runRewriteOn
            (dispatch (chain [collapseApplication, incrementOddAny]))
            simpleApplication
          assertEqual "result" (show (integer 4)) (show result)
          assertEqual "count" 2 count
      ]

  , testGroup "compiled strategies equal their references" $
      let
        scenarios =
          [ ( "shape-changing chain on nested term"
            , allTraversals shapeChangingStep
            , nestedTerm
            )
          , ( "literal chain on nested term"
            , allTraversals literalStep
            , nestedTerm
            )
          , ( "context probe on all constructors"
            , allTraversals contextProbeStep
            , allConstructorsTerm
            )
          , ( "AppArg context tracks the rewritten function"
            , allTraversals appArgProbeStep
            , appArgProbeTerm
            )
          , ( "no-op chain on all constructors"
            , allTraversals noOpStep
            , allConstructorsTerm
            )
          , ( "anyShape member"
            , allTraversals anyShapeStep
            , nestedTerm
            )
          , ( "node-entry member"
            , allTraversals nodeEntryStep
            , nestedTerm
            )
          , ( "mid-traversal conditional"
            , allTraversals conditionalStep
            , nestedTerm
            )
          , ( "embedded nested traversal"
            , allTraversals nestedTraversalStep
            , nestedTerm
            )
          ]
      in
        [ testCase (scenarioName <> ": " <> traversalName) $
            assertAgree traversalName fast reference term
        | (scenarioName, traversals, term) <- scenarios
        , (traversalName, fast, reference) <- traversals
        ]

  , testGroup "compiled strategies equal hand-written compositions"
      [ testCase "topdown chain vs sequential apply chain" $
          -- On this bundle a mid-chain constructor change never reaches a
          -- shape whose members were skipped, so the compiled dispatch also
          -- equals the pre-bucketing sequential chain over ALL members.
          assertAgree "topdown"
            (compile (TopDown shapeChangingStep))
            (Comb.topdownR
              (apply "collapseApplication" collapseApplicationRewrite Comb.>->
               apply "incrementBoth" incrementBothRewrite Comb.>->
               apply "inlineTrivialLet" inlineTrivialLetRewrite))
            nestedTerm
      , testCase "topdown anyShape vs plain rewrite" $
          assertAgree "topdown"
            (compile (TopDown (Chain [incrementOddAny])))
            (Comb.topdownR (apply "incrementOddAny" incrementOddAnyWorker))
            nestedTerm
      ]

  , testGroup "strategy combinators"
      [ testCase "whole-term pass" $
          assertAgree "pass"
            (compile (Pass "incrementOddAny" incrementOddAnyWorker))
            (compileReference (Pass "incrementOddAny" incrementOddAnyWorker))
            nestedTerm
      , testCase "repeatR over sequenced traversals" $
          assertAgree "repeat"
            (compile
               (repeatR (topdown collapseApplication >-> bottomup incrementOdd)))
            (compileReference
               (repeatR (topdown collapseApplication >-> bottomup incrementOdd)))
            nestedTerm
      , testCase "success and failure conditionals between strategies" $ do
          assertAgree "onChange"
            (compile (topdown collapseApplication !-> bottomup incrementOdd))
            (compileReference
               (topdown collapseApplication !-> bottomup incrementOdd))
            nestedTerm
          assertAgree "onNoChange"
            (compile (topdown inlineTrivialLet >-! bottomup incrementOdd))
            (compileReference
               (topdown inlineTrivialLet >-! bottomup incrementOdd))
            nestedTerm
      , testCase "deepseq between strategies" $
          assertAgree "deepseq"
            (compile (topdown collapseApplication >-!-> bottomup incrementOdd))
            (compileReference
               (topdown collapseApplication >-!-> bottomup incrementOdd))
            nestedTerm
      , testCase "calling another compiled strategy" $
          assertAgree "callStrategy"
            (compile
               (callStrategy calleeFast >-> topdown collapseApplication))
            (compileReference
               (callStrategy calleeReference >-> topdown collapseApplication))
            nestedTerm
      ]

  , testGroup "descent"
      [ testCase "NonRec lets are rebuilt as Rec, like allR" $ do
          -- See Note [NonRec erasure during descent] in
          -- Clash.Rewrite.StrategyDSL.Compile.
          let term = Let (NonRec (someId 0) (integer 3)) (integer 5)
          (result, _, _) <- runRewriteOn
            (compile (TopDown literalStep)) term
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
            (compile (TopDown appArgProbeStep))
            appArgProbeTerm
          case result of
            App _function (Literal (StringLiteral recordedContext)) ->
              assertBool ("recorded context mentions somePrim: " <> recordedContext)
                ("somePrim" `List.isInfixOf` recordedContext)
            _ -> assertFailure ("expected a probed argument, got: " <> show result)
      ]

    -- Strategy compilation happens at runtime, so an invalid spec no longer
    -- fails the clash-lib build the way a failing splice did; this group
    -- restores that coverage at unit-test time.
  , testGroup "normalization strategy specs validate"
      [ testCase "constantPropagationSpec" $
          validateStrat constantPropagationSpec @?= Right ()
      , testCase "normalizationSpec" $
          validateStrat (normalizationSpec constantPropagation) @?= Right ()
      , testCase "flattenSpec" $
          validateStrat flattenSpec @?= Right ()
      ]

  , testGroup "asRewrite"
      [ testCase "fires without instrumentation" $ do
          (result, count, changedFlag) <- runRewriteOn
            (asRewrite inlineTrivialLet)
            (Let (NonRec (someId 0) (integer 5)) (Var (someId 0)))
          assertEqual "result" (show (integer 5)) (show result)
          assertEqual "count" 0 count
          assertEqual "changed" True changedFlag
      , testCase "non-matching node untouched" $ do
          (result, count, changedFlag) <- runRewriteOn
            (asRewrite inlineTrivialLet)
            simpleApplication
          assertEqual "result" (show simpleApplication) (show result)
          assertEqual "count" 0 count
          assertEqual "changed" False changedFlag
      ]
  ]
