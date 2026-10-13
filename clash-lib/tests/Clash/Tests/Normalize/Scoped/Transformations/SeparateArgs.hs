{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.SeparateArgs".
  'assertRewritesTo' runs every test in the scope of the free variables of its
  input, and in a scope that the binders of the input clash with.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.SeparateArgs (tests) where

import Data.Default (def)
import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C (nameOcc)
import Clash.Core.Scoped.Alpha (aeqTy)
import Clash.Core.Scoped.HasType (inferCoreTypeOf)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (collectArgs)
import qualified Clash.Normalize.Scoped.Transformations.SeparateArgs as Scoped
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Scoped (Rewrite)
import Clash.Rewrite.StrategyDSL.TH (asScopedRewriteQ)
import Clash.Rewrite.Types (RewriteState)

import Test.Clash.Rewrite
  ( TestDefs (..), TestTerm, parseTestTermWith, parseTestType, rewriteEnvWithTyCons, strQQ
  , testGlobalId )
import Test.Clash.Scoped
  ( ScopeDecl, Step (..), TestScope (..), assertNoRewrite, assertRewritesAt, assertRewritesTo
  , closedType, runScopedAt, runScopedIn, termIn, testTypeEnv, varD, withTestScope )
import Test.Clash.Scoped.Print (showTestType)

-- | Global functions @gp :: Int -> ClkPair -> Int@, @gq :: ClkPair -> Int ->
-- Int@ and @gv :: forall a. a -> Vec 2 (Clock System) -> a@
globals :: [GlobalId]
globals =
  [ testGlobalId "gp_G41" "Int -> ClkPair -> Int"
  , testGlobalId "gq_G44" "ClkPair -> Int -> Int"
  , testGlobalId "gv_G42" "forall (a :: Type). a -> Clash.Sized.Vector.Vec 2 (Clash.Signal.Internal.Clock \"System\") -> a"
  , testGlobalId "g_G47" "forall (a :: Type). ClkPair -> a -> a" ]

tm :: String -> TestTerm
tm = parseTestTermWith (TestDefs [] [] globals)

st :: RewriteState NormalizeState
st = def

separateArgumentsS :: Rewrite NormalizeState
separateArgumentsS = $(asScopedRewriteQ Scoped.separateArguments)

-- | Assert that 'Scoped.separateArguments' rewrites a term to the expected one
separates :: [ScopeDecl] -> TestTerm -> TestTerm -> Assertion
separates = assertRewritesTo rewriteEnvWithTyCons st separateArgumentsS

-- | Assert that 'Scoped.separateArguments' leaves a term as it is
keeps :: [ScopeDecl] -> TestTerm -> Assertion
keeps = assertNoRewrite rewriteEnvWithTyCons st separateArgumentsS

-- | The human readable names of the binders of the lambdas a term starts
-- with, after those at the given path
lamBinderOccs :: [Step] -> Term n -> [Text.Text]
lamBinderOccs path e = case (path, e) of
  ([], Lam b body) -> C.nameOcc (idBinderOcc b) : lamBinderOccs [] body
  ([], _) -> []
  (LetRhsStep 0 : rest, Let (NonRec _ x) _) -> lamBinderOccs rest x
  _ -> []

-- | Assert that 'Scoped.separateArguments' names the binders it creates for
-- the lambda at a path as expected
assertNames :: [Step] -> [ScopeDecl] -> TestTerm -> [Text.Text] -> Assertion
assertNames path decls input expected = withTestScope decls $ \ts -> do
  (e, _, _) <- runScopedIn rewriteEnvWithTyCons st ts separateArgumentsS (termIn ts input)
  assertEqual "the names of the binders" expected (lamBinderOccs path e)

lambdaTests :: TestTree
lambdaTests = testGroup "Lambdas"
  [ testCase "splits the binder of a product type" $ do
      separates [varD "h_40" "ClkPair -> Int"]
        (tm "\\(p :: ClkPair) -> h_40 p")
        (tm "\\(p1 :: Clash.Signal.Internal.Clock \"System\") (p2 :: Int) -> h_40 (MkClkPair p1 p2)")
      assertNames [] [varD "h_40" "ClkPair -> Int"] (tm "\\(p :: ClkPair) -> h_40 p") ["c$p", "c$p"]
    -- The vectors are built with 'Clash.Core.Scoped.Util.primCo'
  , testCase "splits the binder of a vector type" $
      separates []
        (tm "\\(v :: Clash.Sized.Vector.Vec 2 (Clash.Signal.Internal.Clock \"System\")) -> v")
        (tm [strQQ|
          \(v1 :: Clash.Signal.Internal.Clock "System") (v2 :: Clash.Signal.Internal.Clock "System") ->
            Cons @2 @(Clash.Signal.Internal.Clock "System") @1 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1))) v1
              (Cons @1 @(Clash.Signal.Internal.Clock "System") @0 (Prim._CO_ @((GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1))) v2
                (Nil @0 @(Clash.Signal.Internal.Clock "System") (Prim._CO_ @((GHC.Prim.~#) Nat Nat 0 0))))
        |])
  , testCase "names the binders after the closest let-binder" $ do
      let input = tm "let { !(p :: ClkPair -> Int) = \\(p :: ClkPair) -> h_40 p } in p"
      assertRewritesAt rewriteEnvWithTyCons st [LetRhsStep 0] separateArgumentsS
        [varD "h_40" "ClkPair -> Int"] input
        (tm [strQQ|
          let { !(p :: ClkPair -> Int) =
            \(p1 :: Clash.Signal.Internal.Clock "System") (p2 :: Int) -> h_40 (MkClkPair p1 p2)
          } in p
        |])
      withTestScope [varD "h_40" "ClkPair -> Int"] $ \ts -> do
        (e, _, _) <- runScopedAt rewriteEnvWithTyCons st ts [LetRhsStep 0] separateArgumentsS (termIn ts input)
        assertEqual "the names of the binders" ["c$p_p", "c$p_p"] (lamBinderOccs [LetRhsStep 0] e)
  , testCase "doesn't split another binder" $
      keeps [] (tm "\\(x :: Int) -> x")
  ]

spineTests :: TestTree
spineTests = testGroup "Application spines"
  [ testCase "splits the arguments of a global function" $
      separates [varD "p_30" "ClkPair", varD "x_32" "Int"]
        (tm "gp_G41 x_32 p_30")
        (tm [strQQ|
          gp_G41 x_32
            ((case p_30 of { MkClkPair (s :: Clash.Signal.Internal.Clock "System") (w :: Int) -> s }) :: Clash.Signal.Internal.Clock "System")
            ((case p_30 of { MkClkPair (w :: Clash.Signal.Internal.Clock "System") (s :: Int) -> s }) :: Int)
        |])
  , testCase "splits the arguments after type arguments" $
      separates
        [varD "v_31" "Clash.Sized.Vector.Vec 2 (Clash.Signal.Internal.Clock \"System\")", varD "x_32" "Int"]
        (tm "gv_G42 @Int x_32 v_31")
        (tm [strQQ|
          gv_G42 @Int x_32
            ((case v_31 of
              { Cons (m :: Type) (w1 :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (s :: Clash.Signal.Internal.Clock "System")
                  (w2 :: Clash.Sized.Vector.Vec 1 (Clash.Signal.Internal.Clock "System")) -> s
              }) :: Clash.Signal.Internal.Clock "System")
            ((case
                (case v_31 of
                  { Cons (m :: Type) (w1 :: (GHC.Prim.~#) Nat Nat 2 ((GHC.TypeNats.+) 1 1)) (w2 :: Clash.Signal.Internal.Clock "System")
                      (s :: Clash.Sized.Vector.Vec 1 (Clash.Signal.Internal.Clock "System")) -> s
                  }) :: Clash.Sized.Vector.Vec 1 (Clash.Signal.Internal.Clock "System")
              of
              { Cons (m :: Type) (w1 :: (GHC.Prim.~#) Nat Nat 1 ((GHC.TypeNats.+) 0 1)) (s :: Clash.Signal.Internal.Clock "System")
                  (w2 :: Clash.Sized.Vector.Vec 0 (Clash.Signal.Internal.Clock "System")) -> s
              }) :: Clash.Signal.Internal.Clock "System")
        |])
  , testCase "keeps the ticks of the function" $
      separates [varD "p_30" "ClkPair", varD "x_32" "Int"]
        (tm "Core.prefixName @Pfx gp_G41 x_32 p_30")
        (tm [strQQ|
          Core.prefixName @Pfx gp_G41 x_32
            ((case p_30 of { MkClkPair (s :: Clash.Signal.Internal.Clock "System") (w :: Int) -> s }) :: Clash.Signal.Internal.Clock "System")
            ((case p_30 of { MkClkPair (w :: Clash.Signal.Internal.Clock "System") (s :: Int) -> s }) :: Int)
        |])
  , testCase "splits the arguments of a partial application" $
      separates [varD "p_30" "ClkPair"]
        (tm "gq_G44 p_30")
        (tm [strQQ|
          gq_G44
            ((case p_30 of { MkClkPair (s :: Clash.Signal.Internal.Clock "System") (w :: Int) -> s }) :: Clash.Signal.Internal.Clock "System")
            ((case p_30 of { MkClkPair (w :: Clash.Signal.Internal.Clock "System") (s :: Int) -> s }) :: Int)
        |])
  , testCase "doesn't split the arguments of a local function" $
      keeps [varD "p_30" "ClkPair", varD "h_40" "ClkPair -> Int"] (tm "h_40 p_30")
  ]

-- | Run 'Scoped.separateArguments' on an application of a global function,
-- with the given variables in scope, and assert that it gives the global function the
-- expected type, and that the type of the application doesn't change
assertGlobalType :: [ScopeDecl] -> String -> TestTerm -> Assertion
assertGlobalType decls expected input = withTestScope decls $ \ts -> do
  let e = termIn ts input
  (res, _, _) <- runScopedIn rewriteEnvWithTyCons st ts separateArgumentsS e
  case collectArgs res of
    (GlobalVar g, _) ->
      assertBool
        ("Expected the type\n" <> expected <> "\nfor the global, but got:\n"
          <> showTestType (globalIdType g))
        (aeqTy (closedType (parseTestType expected)) (globalIdType g))
    _ -> assertFailure "Expected an application of a global"
  let ty = inferCoreTypeOf testTypeEnv (testScope ts)
  assertBool
    ("The type\n" <> showTestType (ty e) <> "\nchanged to\n" <> showTestType (ty res))
    (aeqTy (ty e) (ty res))

globalTypeTests :: TestTree
globalTypeTests = testGroup "The types of globals"
  [ -- @gp :: Int -> ClkPair -> Int@ becomes @gp :: Int -> Clock -> Int -> Int@,
    -- not @gp :: Int -> ClkPair -> ClkPair -> Int@
    testCase "gives the global the types of the fields" $
      assertGlobalType [varD "p_30" "ClkPair", varD "x_32" "Int"]
        "Int -> Clash.Signal.Internal.Clock \"System\" -> Int -> Int"
        (tm "gp_G41 x_32 p_30")
    -- @gq :: ClkPair -> Int -> Int@ becomes @gq :: Clock -> Int -> Int -> Int@,
    -- also when it isn't applied to all of its arguments
  , testCase "keeps the parameters that aren't applied" $
      assertGlobalType [varD "p_30" "ClkPair"]
        "Clash.Signal.Internal.Clock \"System\" -> Int -> Int -> Int"
        (tm "gq_G44 p_30")
    -- @g \@(Int -> Int) p f x@ for @g :: forall a. ClkPair -> a -> a@ keeps
    -- the argument @x@, which is applied to the result of @g@
  , testCase "keeps the arguments applied to the result" $
      assertGlobalType [varD "p_30" "ClkPair", varD "x_32" "Int", varD "f_48" "Int -> Int"]
        "forall (a :: Type). Clash.Signal.Internal.Clock \"System\" -> Int -> a -> a"
        (tm "g_G47 @(Int -> Int) p_30 f_48 x_32")
  ]

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.SeparateArgs"
  [ lambdaTests
  , spineTests
  , globalTypeTests
  ]
