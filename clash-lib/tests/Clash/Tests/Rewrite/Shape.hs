{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Shape": the shape-dispatch chain semantics (bucket
  order, the guarded suffix after a mid-chain constructor change) and the
  equivalence of the fused traversals with their unfused reference
  compositions built from 'Clash.Rewrite.Combinators'.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Rewrite.Shape (tests) where

import qualified Data.List as List
import qualified Data.Monoid as Monoid
import Data.Default (def)

import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (mkUnsafeSystemName)
import Clash.Core.Term
  (Bind (..), IsMultiPrim (..), Pat (..), PrimInfo (..), PrimUnfolding (..),
   Term (..), TickInfo (..), WorkInfo (..))
import Clash.Core.Type (ConstTy (..), Type (..))
import Clash.Core.Var (Id, mkLocalId, mkTyVar)
import Clash.Core.VarEnv (emptyInScopeSet)
import Clash.Normalize.Strategy (innerMost, topdownSucR)
import Clash.Rewrite.Combinators (bottomupR, topdownFixR, topdownR)
import Clash.Rewrite.Shape
import Clash.Rewrite.Types
  (Rewrite, RewriteState (..), TransformContext (..), runR)
import Clash.Rewrite.Util (apply, changed)
import Clash.Unique (Unique)
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

-- * Term construction helpers

someType :: Type
someType = ConstTy (TyCon (mkUnsafeSystemName "T" 0))

someId :: Unique -> Id
someId unique = mkLocalId someType (mkUnsafeSystemName "x" unique)

somePrim :: PrimInfo
somePrim = PrimInfo
  { primName = "somePrim"
  , primType = someType
  , primWorkInfo = WorkConstant
  , primMultiResult = SingleResult
  , primUnfolding = NoUnfolding
  }

integer :: Integer -> Term
integer = Literal . IntLiteral

-- * Stub transformations
--
-- Every stub exists in two forms: a 'ShapedTransformation' and the equivalent
-- old-style 'Rewrite' with its own constructor match and catch-all.

-- | @f 3@ becomes @3@: fires on an application whose argument is an integer
-- literal, changing the node's constructor.
collapseApplicationShaped :: ShapedTransformation ()
collapseApplicationShaped = applyApp "collapseApplication" go
 where
  go _ctx _node _function argument@(Literal IntLiteral{}) = changed argument
  go _ctx node _function _argument = pure node

collapseApplicationRewrite :: Rewrite ()
collapseApplicationRewrite _ctx (App _function argument@(Literal IntLiteral{})) =
  changed argument
collapseApplicationRewrite _ctx term = pure term

-- | Increments odd integer literals (even results keep repeated traversals
-- terminating).
incrementOddShaped :: ShapedTransformation ()
incrementOddShaped = applyShapes "incrementOdd" (onLiteral go)
 where
  go _ctx _node (IntLiteral i) | odd i = changed (integer (i + 1))
  go _ctx node _literal = pure node

-- | Multi-shape: increments an odd integer literal, either as the argument of
-- an application or bare. Registered for App and Literal, to observe the
-- guarded suffix.
incrementBothShaped :: ShapedTransformation ()
incrementBothShaped =
  applyShapes "incrementBoth" (onApp goApplication <> onLiteral goLiteral)
 where
  goApplication _ctx _node function (Literal (IntLiteral i))
    | odd i = changed (App function (integer (i + 1)))
  goApplication _ctx node _function _argument = pure node
  goLiteral _ctx _node (IntLiteral i) | odd i = changed (integer (i + 1))
  goLiteral _ctx node _literal = pure node

incrementBothRewrite :: Rewrite ()
incrementBothRewrite _ctx (App function (Literal (IntLiteral i)))
  | odd i = changed (App function (integer (i + 1)))
incrementBothRewrite _ctx (Literal (IntLiteral i)) | odd i = changed (integer (i + 1))
incrementBothRewrite _ctx term = pure term

-- | Inlines a let binder into its body when the body is exactly that
-- variable: @let x = e in x@ becomes @e@. Fires on 'Let'.
inlineTrivialLetShaped :: ShapedTransformation ()
inlineTrivialLetShaped = applyLet "inlineTrivialLet" go
 where
  go _ctx node bind body = case (bind, body) of
    (NonRec i boundTerm, Var v) | v == i -> changed boundTerm
    (Rec [(i, boundTerm)], Var v) | v == i -> changed boundTerm
    _ -> pure node

inlineTrivialLetRewrite :: Rewrite ()
inlineTrivialLetRewrite _ctx node@(Let bind body) = case (bind, body) of
  (NonRec i boundTerm, Var v) | v == i -> changed boundTerm
  (Rec [(i, boundTerm)], Var v) | v == i -> changed boundTerm
  _ -> pure node
inlineTrivialLetRewrite _ctx term = pure term

-- | Replaces variable references with a string literal spelling out the full
-- 'Clash.Core.Term.CoreContext' path, exposing every context push of the
-- descent arms to the equivalence tests.
reifyContextShaped :: ShapedTransformation ()
reifyContextShaped = applyVar "reifyContext" go
 where
  go ctx _node _i = changed (Literal (StringLiteral (show (tfContext ctx))))

-- | Rewrites references to one specific variable to a primitive, so the
-- 'Clash.Core.Term.AppArg' context a sibling argument sees is computed from
-- an already rewritten function.
functionToPrimShaped :: ShapedTransformation ()
functionToPrimShaped = applyVar "functionToPrim" go
 where
  go _ctx node i
    | i == someId 2 = changed (Prim somePrim)
    | otherwise = pure node

-- * Test terms

-- | @(let x = f 3 in x) (let y = 5 in y)@ — exercises App, Let (both binder
-- and body positions), and literals at several depths.
nestedTerm :: Term
nestedTerm =
  App
    (Let (NonRec x (App (Var f) (integer 3))) (Var x))
    (Let (Rec [(y, integer 5)]) (Var y))
 where
  x = someId 0
  y = someId 1
  f = someId 2

-- | A term covering every recursive constructor: Lam, TyLam, App, TyApp,
-- Let, Case (subject and alternatives), Cast, and Tick.
allConstructorsTerm :: Term
allConstructorsTerm =
  Lam lamBinder
    (TyLam tyVariable
      (Tick DeDup
        (Case
          (Cast (App (Var lamBinder) subject) someType someType)
          someType
          [ (DefaultPat, integer 3)
          , (LitPat (IntLiteral 0), TyApp (Var lamBinder) someType)
          ])))
 where
  lamBinder = someId 3
  tyVariable = mkTyVar someType (mkUnsafeSystemName "a" 100)
  subject = Let (NonRec (someId 5) (integer 7)) (Var (someId 5))

simpleApplication :: Term
simpleApplication = App (Var (someId 6)) (integer 3)

-- | The function position rewrites to a primitive; the argument's probed
-- context must show the 'Clash.Core.Term.AppArg' of that primitive.
appArgProbeTerm :: Term
appArgProbeTerm = App (Var (someId 2)) (Var (someId 7))

-- * Reference compositions

-- | Each fused traversal next to its unfused reference: the same compiled
-- bundle dispatched as a plain 'Rewrite' inside the existing combinators.
fusedAndReference
  :: [ShapedTransformation ()]
  -> [(String, Rewrite (), Rewrite ())]
fusedAndReference shaped =
  [ ("topdown", topdownBundle bundle, topdownR (dispatchBundle bundle))
  , ("bottomup", bottomupBundle bundle, bottomupR (dispatchBundle bundle))
  , ("topdownFix", topdownFixBundle bundle, topdownFixR (dispatchBundle bundle))
  , ("topdownSuc", topdownSucBundle bundle, topdownSucR (dispatchBundle bundle))
  , ("innerMost", innerMostBundle bundle, innerMost (dispatchBundle bundle))
  ]
 where
  bundle = compileBundle shaped

-- * Tests

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Shape"
  [ testGroup "singleton dispatch equals apply"
      [ testCase "Let transformation fires" $
          assertAgree "inlineTrivialLet"
            (runShapedTransformation inlineTrivialLetShaped)
            (apply "inlineTrivialLet" inlineTrivialLetRewrite)
            (Let (NonRec (someId 0) (integer 5)) (Var (someId 0)))
      , testCase "Let transformation offered a non-Let node" $
          assertAgree "inlineTrivialLet"
            (runShapedTransformation inlineTrivialLetShaped)
            (apply "inlineTrivialLet" inlineTrivialLetRewrite)
            simpleApplication
      , testCase "App transformation fires" $
          assertAgree "collapseApplication"
            (runShapedTransformation collapseApplicationShaped)
            (apply "collapseApplication" collapseApplicationRewrite)
            simpleApplication
      , testCase "multi-shape transformation, both shapes" $ do
          assertAgree "incrementBoth on App"
            (runShapedTransformation incrementBothShaped)
            (apply "incrementBoth" incrementBothRewrite)
            simpleApplication
          assertAgree "incrementBoth on Literal"
            (runShapedTransformation incrementBothShaped)
            (apply "incrementBoth" incrementBothRewrite)
            (integer 3)
      ]

  , testGroup "bucket chain semantics"
      [ testCase "guarded suffix: later same-bucket member fires on changed shape" $ do
          -- collapseApplication turns @f 3@ into @3@; incrementBoth is later
          -- in the App bucket and must still fire — on the literal, through
          -- its own Literal handler.
          (result, count, changedFlag) <- runRewriteOn
            (dispatchBundle (compileBundle
              [collapseApplicationShaped, incrementBothShaped]))
            simpleApplication
          assertEqual "result" (show (integer 4)) (show result)
          assertEqual "count" 2 count
          assertEqual "changed" True changedFlag
      , testCase "members of other buckets do not run in the same pass" $ do
          -- incrementOdd is registered for Literal only; after
          -- collapseApplication changes App to Literal it must NOT run in
          -- this dispatch pass — it was never in the App bucket.
          (result, count, _) <- runRewriteOn
            (dispatchBundle (compileBundle
              [collapseApplicationShaped, incrementOddShaped]))
            simpleApplication
          assertEqual "result" (show (integer 3)) (show result)
          assertEqual "count" 1 count
      , testCase "earlier members do not rerun after a change" $ do
          -- incrementBoth (App handler) fires first, turning @f 3@ into
          -- @f 4@; collapseApplication is later in the App bucket and
          -- collapses to @4@. incrementBoth must not see the literal again.
          (result, count, _) <- runRewriteOn
            (dispatchBundle (compileBundle
              [incrementBothShaped, collapseApplicationShaped]))
            simpleApplication
          assertEqual "result" (show (integer 4)) (show result)
          assertEqual "count" 2 count
      , testCase "no members for the node's constructor: untouched, no change" $ do
          (result, count, changedFlag) <- runRewriteOn
            (dispatchBundle (compileBundle [inlineTrivialLetShaped]))
            simpleApplication
          assertEqual "result" (show simpleApplication) (show result)
          assertEqual "count" 0 count
          assertEqual "changed" False changedFlag
      ]

  , testGroup "fused traversals equal reference compositions" $
      let
        scenarios =
          [ ( "shape-changing bundle on nested term"
            , [collapseApplicationShaped, incrementBothShaped, inlineTrivialLetShaped]
            , nestedTerm
            )
          , ( "literal bundle on nested term"
            , [incrementOddShaped]
            , nestedTerm
            )
          , ( "context probe on all constructors"
            , [reifyContextShaped]
            , allConstructorsTerm
            )
          , ( "AppArg context tracks the rewritten function"
            , [functionToPrimShaped, reifyContextShaped]
            , appArgProbeTerm
            )
          , ( "no-op bundle on all constructors"
            , [inlineTrivialLetShaped]
            , allConstructorsTerm
            )
          ]
      in
        [ testCase (scenarioName <> ": " <> traversalName) $
            assertAgree traversalName fused reference term
        | (scenarioName, bundleMembers, term) <- scenarios
        , (traversalName, fused, reference) <- fusedAndReference bundleMembers
        ]

  , testGroup "descent"
      [ testCase "NonRec lets are rebuilt as Rec, like allR" $ do
          -- See Note [NonRec erasure during descent] in Clash.Rewrite.Shape.
          let term = Let (NonRec (someId 0) (integer 3)) (integer 5)
          (result, _, _) <- runRewriteOn
            (topdownBundle (compileBundle [incrementOddShaped])) term
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
            (topdownBundle (compileBundle
              [functionToPrimShaped, reifyContextShaped]))
            appArgProbeTerm
          case result of
            App _function (Literal (StringLiteral recordedContext)) ->
              assertBool ("recorded context mentions somePrim: " <> recordedContext)
                ("somePrim" `List.isInfixOf` recordedContext)
            _ -> assertFailure ("expected a probed argument, got: " <> show result)
      ]
  ]
