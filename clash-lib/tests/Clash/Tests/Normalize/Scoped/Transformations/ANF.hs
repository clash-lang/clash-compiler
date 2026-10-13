{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.ANF".

  'assertRewritesTo' runs the regression tests in the scope of the free
  variables of their inputs, and in a scope that the binders of their inputs
  clash with.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.ANF (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.DataCon as C
import Clash.Core.Scoped.Syntax (GlobalId, Term (App, Data, GlobalVar, Prim))
import Clash.Driver.Types (Binding (..))
import qualified Clash.Normalize.Scoped.Transformations.ANF as Scoped
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Scoped (ScopedRewrite, ScopedTransform (..))
import Clash.Rewrite.StrategyDSL.TH (asScopedRewriteQ)
import Clash.Rewrite.Types (RewriteState)

import Test.Clash.Rewrite
  ( TestDefs (..), TestTerm, kindedTyVarD, parseTestTermWith, rewriteEnvWithTyCons
  , strQQ, testDataCon, testGlobalId, varD )
import Test.Clash.Scoped
  ( assertCaptureRewritesTo, assertLintClean, assertNoRewrite, assertRewritesTo, assertTermIs
  , closedTerm, emptyTestScope, newBindings, runScopedIn, termIn, withCurFun )
import Test.Clash.Scoped.Print (showTestTerm)

-- * Definitions

-- | @MkP :: Int -> (Int -> Int) -> P@
mkP :: C.DataCon
mkP = testDataCon "MkP" 1001 1 [] [] ["Int", "Int -> Int"] "P"

-- | @(:-) :: Int -> Sig -> Sig@, a stand-in for the constructor of
-- @Signal@. ANF recognizes it by its name.
signalCons :: C.DataCon
signalCons = testDataCon "Clash.Signal.Internal.:-" 1101 1 [] [] ["Int", "Sig"] "Sig"

-- | @MkE :: forall m. m -> Int -> E@
mkE :: C.DataCon
mkE = testDataCon "MkE" 1202 1 [] [kindedTyVarD "m" "Type"] ["m", "Int"] "E"

-- | @MkE :: forall e. Int -> E@, whose existential type variable doesn't
-- occur in its fields
mkEInt :: C.DataCon
mkEInt = testDataCon "MkE" 1302 1 [] [kindedTyVarD "e" "Type"] ["Int"] "E"

-- | Global functions @k :: Int -> Int -> Int@, @h :: Int -> Int@,
-- @fHo :: (Int -> Int) -> Int@ and @g :: forall c. Int -> Int@
globals :: [GlobalId]
globals =
  [ testGlobalId "k_G33" "Int -> Int -> Int"
  , testGlobalId "h_G50" "Int -> Int"
  , testGlobalId "fHo_G51" "(Int -> Int) -> Int"
  , testGlobalId "g_G1306" "forall (c :: Type). Int -> Int" ]

tm :: String -> TestTerm
tm = parseTestTermWith (TestDefs [mkP, signalCons, mkE] [] globals)

st :: RewriteState NormalizeState
st = def

anfS, nonRepANFS :: ScopedRewrite NormalizeState
anfS = ScopedTransform Scoped.makeANF
nonRepANFS = $(asScopedRewriteQ Scoped.nonRepANF)

-- | Assert that 'Scoped.makeANF' rewrites a closed term to the expected one,
-- with the type constructors of 'Test.Clash.Rewrite.testTyConMap'
anfTo :: String -> String -> Assertion
anfTo input expected = assertRewritesTo rewriteEnvWithTyCons st anfS [] (tm input) (tm expected)

-- | Assert that 'Scoped.makeANF' leaves a closed term as it is
anfKeeps :: TestTerm -> Assertion
anfKeeps = assertNoRewrite rewriteEnvWithTyCons st anfS []

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Scoped.Transformations.ANF"
    [ testGroup "regression tests" regressionTests
    , testGroup "makeANF" makeANFTests
    , testCase "nonRepANF keeps a lambda that refers to a let-binder in place" $
        assertNoRewrite rewriteEnvWithTyCons st nonRepANFS []
          (tm "\\(x :: Int) -> MkP x (let { !(a :: Int) = h_G50 x } in \\(z :: Int) -> k_G33 a z)")
    , testCase "makeANF keeps let-bindings under a lambda" $
        anfKeeps (tm "\\(x :: Int) -> fHo_G51 (\\(z :: Int) -> k_G33 (h_G50 z) z)")
      -- An alternative whose existential type variable doesn't occur in the
      -- types of its fields, but in its right-hand side, keeps its
      -- let-bindings, like an alternative that binds existential type
      -- variables
    , testCase "makeANF keeps let-bindings in an alternative that mentions its existential type variable" $
        anfKeeps $ parseTestTermWith (TestDefs [mkEInt] [] globals)
          "\\(s :: E) -> (case s of { MkE (e :: Type) (y :: Int) -> k_G33 (g_G1306 @e y) y }) :: Int"
    ]

-- | Regression tests for bugs in the conversion to ANF
regressionTests :: [TestTree]
regressionTests =
  [ -- 'nonRepANF' moved the let-bindings out of a non-representable argument
    -- of a data constructor without renaming them. They could then capture
    -- free variables of the other arguments: here the binder @x@ would
    -- capture the free variable @x_2@.
    -- https://github.com/clash-lang/clash-compiler/pull/1071
    testCase "nonRepANF deshadows let-bindings (#1071)" $ do
      assertRewritesTo def st nonRepANFS [varD "u_1" "Int", varD "x_2" "Int"]
        (tm "MkP x_2 (let { !(x :: Int) = u_1 } in \\(y :: Int) -> x)")
        (tm "let { !(x :: Int) = u_1 } in MkP x_2 (\\(y :: Int) -> x)")
      -- The let-binder @x@ has the name of the free @x@
      assertCaptureRewritesTo def st nonRepANFS [varD "u_1" "Int"]
        (tm "let { !(x :: Int) = u_1 } in \\(y :: Int) -> x")
        [varD "x_2" "Int"]
        (\ts arg -> App (App (Data mkP) (termIn ts (tm "x_2"))) arg)
        (tm "let { !(x :: Int) = u_1 } in MkP x_2 (\\(y :: Int) -> x)")

    -- ANF creates new let-binders and moves all existing let-binders into a
    -- single let-expression. It used to pick uniques for the new binders that
    -- only avoided the free variables, not the bound variables of the
    -- expression, so a new binder could get the same unique as an existing
    -- one. In scoped Core, the binders it creates are fresh by construction.
    -- https://github.com/clash-lang/clash-compiler/commit/63827c31fff4ab27005f25cb027969f8805d646d
  , testCase "ANF does not duplicate binders" $
      assertRewritesTo def st anfS [varD "x_31" "Int"]
        (tm "k_G33 (h_G50 x_31) (let { (b :: Int) = 3 } in b)")
        (tm "let { (b :: Int) = 3; (a :: Int) = h_G50 x_31 } in k_G33 a b")

    -- ANF moves all let-bindings it creates to the root of the term. It used
    -- to also let-bind expressions referring to local variables that would
    -- not end up in scope there, introducing free variables. Today, ANF relies
    -- on 'nonRepANF', which runs right before it, to turn non-representable
    -- arguments of primitives and constructors, such as lambdas, into
    -- applications of new global functions. This checks that the two
    -- together don't let-bind @h z@ in @p (\\z -> k (h z) z)@ outside of the
    -- lambda binding @z@.
    -- https://github.com/clash-lang/clash-compiler/commit/36d60f54bf90f03b0cbf2d291f6411252c86dc7f
  , testCase "ANF does not lift expressions out of lambdas" $ do
      let st0 = withCurFun (testGlobalId "top_G42" "Int -> Int") st
          e = closedTerm (tm "(Prim.p :: (Int -> Int) -> Int) (\\(z :: Int) -> k_G33 (h_G50 z) z)")
      (e1, st1, ch1) <- runScopedIn def st0 emptyTestScope nonRepANFS e
      (e2, _, _) <- runScopedIn def st1 emptyTestScope anfS e1
      ch1 @?= True
      assertLintClean emptyTestScope e2
      case newBindings st0 st1 of
        [b] -> do
          case e2 of
            App (Prim _) (GlobalVar g) | g == bindingId b -> pure ()
            _ -> assertFailure ("unexpected result: " <> showTestTerm e2)
          assertTermIs emptyTestScope (tm "\\(z :: Int) -> k_G33 (h_G50 z) z") (bindingTerm b)
        bs -> assertFailure ("expected a single new binding, got " <> show (length bs))

    -- ANF used to leave case-expressions with an alternative for the @(:-)@
    -- constructor of @Signal@ in place, but still let-bound the subterms of
    -- that alternative outside of it. Here, it let-bound @h x@ outside of the
    -- alternative binding @x@.
  , testCase "ANF does not lift expressions out of Signal (:-) alternatives" $
      anfTo
        "\\(s :: Sig) -> (case s of { (Clash.Signal.Internal.:-) (x :: Int) (xs :: Sig) -> k_G33 (h_G50 x) x }) :: Int"
        [strQQ|
          \(s :: Sig) ->
            let
              { (x :: Int) = (case s of { (Clash.Signal.Internal.:-) (a :: Int) (b :: Sig) -> a }) :: Int
              ; (xs :: Sig) = (case s of { (Clash.Signal.Internal.:-) (a :: Int) (b :: Sig) -> b }) :: Sig
              ; (r :: Int) = k_G33 y x
              ; (y :: Int) = h_G50 x
              } in r
        |]

    -- ANF leaves alternatives whose pattern binds existential type variables
    -- in place, but used to let-bind the subterms of these alternatives
    -- outside of them. Here, it let-bound @h y@ outside of the alternative
    -- binding @y@.
  , testCase "ANF does not lift expressions out of existential alternatives" $
      anfKeeps
        (tm "\\(e :: E) -> (case e of { MkE (m :: Type) (x :: m) (y :: Int) -> k_G33 (h_G50 y) y }) :: Int")
  ]

-- | Tests of the shape of the result of 'Scoped.makeANF'
makeANFTests :: [TestTree]
makeANFTests =
  [ testCase "pattern variable used by a let-bound alternative" $
      anfTo
        "\\(m :: Maybe Int) -> (case m of { Nothing -> 0; Just (a :: Int) -> k_G33 (h_G50 a) a }) :: Int"
        [strQQ|
          \(m :: Maybe Int) ->
            let
              { (a :: Int) = (case m of { Just (s :: Int) -> s }) :: Int
              ; (alt :: Int) = k_G33 arg a
              ; (arg :: Int) = h_G50 a
              } in (case m of { Nothing -> 0; Just (a2 :: Int) -> alt }) :: Int
        |]
  , testCase "pattern variable as the right-hand side of one of two alternatives" $
      anfTo
        "\\(m :: Maybe Int) -> (case m of { Nothing -> 0; Just (a :: Int) -> a }) :: Int"
        [strQQ|
          \(m :: Maybe Int) ->
            let
              { (a :: Int) = (case m of { Just (s :: Int) -> s }) :: Int
              ; (alt :: Int) = a
              } in (case m of { Nothing -> 0; Just (a2 :: Int) -> alt }) :: Int
        |]
  , testCase "pattern variable as the right-hand side of a single alternative" $
      anfTo
        "\\(p :: Pair) -> (case p of { MkPair (a :: Int) (b :: Int) -> b }) :: Int"
        [strQQ|
          \(p :: Pair) ->
            let
              { (a :: Int) = (case p of { MkPair (s :: Int) (w :: Int) -> s }) :: Int
              ; (b :: Int) = (case p of { MkPair (w :: Int) (s :: Int) -> s }) :: Int
              } in (case p of { MkPair (a2 :: Int) (b2 :: Int) -> b2 }) :: Int
        |]
  , testCase "single alternative that doesn't use its pattern variables" $
      anfTo
        "\\(x :: Int) (p :: Pair) -> (case p of { MkPair (a :: Int) (b :: Int) -> k_G33 x x }) :: Int"
        [strQQ|
          \(x :: Int) (p :: Pair) ->
            let
              { (a :: Int) = (case p of { MkPair (s :: Int) (w :: Int) -> s }) :: Int
              ; (b :: Int) = (case p of { MkPair (w :: Int) (s :: Int) -> s }) :: Int
              ; (alt :: Int) = k_G33 x x
              } in alt
        |]
    -- @Cons \@m co x xs@, whose existential type variable @m@ doesn't occur in
    -- the types of the fields, as when the length of the vector is known
  , testCase "existential type variable that doesn't occur in the fields" $
      anfTo
        [strQQ|
          \(v :: Clash.Sized.Vector.Vec 2 Int) (x :: Int) ->
            (case v of
              { Cons (m :: Type) (co :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (a :: Int) (xs :: Clash.Sized.Vector.Vec 1 Int) ->
                  k_G33 (h_G50 a) x
              }) :: Int
        |]
        [strQQ|
          \(v :: Clash.Sized.Vector.Vec 2 Int) (x :: Int) ->
            let
              { (co :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) =
                  (case v of { Cons (m :: Type) (s :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (w1 :: Int) (w2 :: Clash.Sized.Vector.Vec 1 Int) -> s })
                    :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)
              ; (a :: Int) =
                  (case v of { Cons (m :: Type) (w0 :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (s :: Int) (w2 :: Clash.Sized.Vector.Vec 1 Int) -> s }) :: Int
              ; (xs :: Clash.Sized.Vector.Vec 1 Int) =
                  (case v of { Cons (m :: Type) (w0 :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (w1 :: Int) (s :: Clash.Sized.Vector.Vec 1 Int) -> s })
                    :: Clash.Sized.Vector.Vec 1 Int
              ; (alt :: Int) = k_G33 arg x
              ; (arg :: Int) = h_G50 a
              } in
                (case v of
                  { Cons (m :: Type) (co2 :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (a2 :: Int) (xs2 :: Clash.Sized.Vector.Vec 1 Int) -> alt
                  }) :: Int
        |]
  , testCase "nested let-expressions" $
      anfTo
        "\\(x :: Int) -> let { !(a :: Int) = h_G50 x; !(b :: Int) = k_G33 a (h_G50 a) } in k_G33 b (h_G50 b)"
        [strQQ|
          \(x :: Int) ->
            let
              { (a :: Int) = h_G50 x
              ; (b :: Int) = k_G33 a arg2
              ; (r :: Int) = k_G33 b arg1
              ; (arg1 :: Int) = h_G50 b
              ; (arg2 :: Int) = h_G50 a
              } in r
        |]
  , testCase "recursive let-expression" $
      anfTo
        "\\(x :: Int) -> let { (a :: Int) = k_G33 b x; (b :: Int) = h_G50 a } in k_G33 a b"
        "\\(x :: Int) -> let { (a :: Int) = k_G33 b x; (b :: Int) = h_G50 a; (r :: Int) = k_G33 a b } in r"
  , testCase "let-bound subject" $
      anfTo
        "\\(x :: Int) -> (case h_G50 x of { _ -> k_G33 x (h_G50 x) }) :: Int"
        [strQQ|
          \(x :: Int) ->
            let
              { (subj :: Int) = h_G50 x
              ; (alt :: Int) = k_G33 x arg
              ; (arg :: Int) = h_G50 x
              } in (case subj of { _ -> alt }) :: Int
        |]
  , testCase "let-expression in an argument" $
      anfTo
        "\\(x :: Int) -> k_G33 (let { !(n :: Int) = h_G50 x } in k_G33 n n) x"
        "\\(x :: Int) -> let { (n :: Int) = h_G50 x; (r :: Int) = k_G33 n n } in k_G33 r x"
  , testCase "name modifier around the term" $
      anfTo
        "\\(x :: Int) -> Core.prefixName @\"pre\" (let { !(a :: Int) = h_G50 x } in k_G33 a a)"
        "\\(x :: Int) -> Core.prefixName @\"pre\" (let { (a :: Int) = h_G50 x; (r :: Int) = k_G33 a a } in r)"
  , testCase "no let-bindings" $
      anfKeeps (tm "\\(x :: Int) -> k_G33 x 1")
  ]
