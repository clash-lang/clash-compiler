{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Inline".

  'assertRewritesTo' runs every test in the scope of the free variables of
  its input, and in a scope that the binders of the input clash with.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.Inline (tests) where

import Data.Default (def)
import GHC.Types.Basic (InlineSpec (Opaque))
import GHC.Types.SourceText (SourceText (NoSourceText))

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Basic as C (IsMultiPrim (SingleResult), WorkInfo (WorkIdentity))
import Clash.Driver.Types (Binding (..))
import qualified Clash.Normalize.Scoped.Transformations.Inline as S
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Scoped (Rewrite)
import Clash.Rewrite.StrategyDSL.TH (asScopedRewriteQ)
import Clash.Rewrite.Types (RewriteEnv, RewriteState (..))

import Clash.Core.Scoped.FreeVars (freeTmVarOccurrences)
import Clash.Core.Scoped.NameMap (GlobalEnv, emptyNameMap, globalEnvFromList, nameMapFromList)
import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped.Util (LetBinding (..), keepBindings, openLetBindings)

import Test.Clash.Rewrite
  ( TestDefs (..), TestTerm, mkGlobalBinding, parseTestTermWith
  , strQQ, testGlobalId, testTyConMap, withTyConMap )
import Test.Clash.Scoped
  ( ScopeDecl, TestScope (..), assertNoRewrite, assertRewritesTo, assertTermIs, closedTerm
  , closedType, mkGlobalBindings, varD, withBinderClashes, withCurFun, withTerm )

-- * Definitions

-- | The function being normalized, a global with a binding, and the other
-- global functions of the tests
top, g, fun, act, both :: GlobalId
top = testGlobalId "top_G10" "Int"
g = testGlobalId "g_G11" "Int"
fun = testGlobalId "fun_G12" "Int -> Int"
act = testGlobalId "act_G13" "Int -> Clash.Explicit.SimIO.SimIO_300 Int"
both = testGlobalId "both_G14"
  "Clash.Explicit.SimIO.SimIO_300 Int -> Clash.Explicit.SimIO.SimIO_300 Int -> Clash.Explicit.SimIO.SimIO_300 Int"

-- | The global @f :: Int -> Int@ of the regression tests
fGlobal :: GlobalId
fGlobal = testGlobalId "f_G7" "Int -> Int"

-- | A primitive like @map :: (Int -> Int) -> Int -> Int@: the identity on its
-- second argument, if its first argument is a no-op
mapPrim :: PrimInfo
mapPrim = mkPrimInfo "mapPrim" (closedType "(Int -> Int) -> Int -> Int") (C.WorkIdentity 1 [0]) C.SingleResult NoUnfolding

-- | A primitive that is the identity on its argument
idPrim :: PrimInfo
idPrim = mkPrimInfo "idPrim" (closedType "Int -> Int") (C.WorkIdentity 0 []) C.SingleResult NoUnfolding

tm :: String -> TestTerm
tm = parseTestTermWith (TestDefs [] [mapPrim, idPrim] [top, g, fun, act, both, fGlobal])

env :: RewriteEnv
env = withTyConMap testTyConMap def

-- | A state in which 'top' is being normalized, with the given bindings, and
-- a binding of 'g'
topState :: GlobalEnv (Binding (Term 'VoidS)) -> RewriteState NormalizeState
topState bs = withCurFun top def
  { _bindings = bs <> mkGlobalBindings [(g, "1")] }

-- | The binding of 'top', as an OPAQUE function. 'collapseRHSNoops' only runs
-- in NOINLINE and OPAQUE functions.
topOpaque :: GlobalEnv (Binding (Term 'VoidS))
topOpaque = globalEnvFromList
  [(top, (mkGlobalBinding top (closedTerm "0")) {bindingSpec = Opaque NoSourceText})]

-- | Assert that a transformation, in a state set up with 'topState', rewrites
-- a term to the expected one
rewritesInTop
  :: GlobalEnv (Binding (Term 'VoidS)) -> Rewrite NormalizeState -> [ScopeDecl]
  -> TestTerm -> TestTerm -> Assertion
rewritesInTop bs = assertRewritesTo env (topState bs)

-- | Assert that a transformation, in a state set up with 'topState', leaves a
-- term as it is
keepsInTop
  :: GlobalEnv (Binding (Term 'VoidS)) -> Rewrite NormalizeState -> [ScopeDecl]
  -> TestTerm -> Assertion
keepsInTop bs = assertNoRewrite env (topState bs)

-- * inlineBndrsCleanup

-- | Run the scoped 'S.inlineBndrsCleanup' on a let-expression, keeping its
-- first binding and inlining the others, as in the regression tests of
-- #1337. Asserts that it gives the let-expression with the expected bindings,
-- also in a scope that its binders clash with.
inlineBndrsCleanup1337 :: TestTerm -> TestTerm -> Assertion
inlineBndrsCleanup1337 expected input =
  withTerm input $ \ts e -> do
    go ts e
    withBinderClashes ts e go
 where
  go :: Distinct n => TestScope n -> Term n -> Assertion
  go ts (Let bind body) = openLetBindings (testScope ts) bind body $ \scL bs lbs body1 ->
    case [(lb, freeTmVarOccurrences (lbRhs lb)) | lb <- lbs] of
      keep0 : inlines ->
        let keep1 = S.inlineBndrsCleanup scL
                      (nameMapFromList [(lbName lb, i) | i@(lb, _) <- inlines])
                      emptyNameMap
                      [keep0]
        in  case keepBindings (testScope ts) bs keep1 body1 of
              Just e -> assertTermIs ts expected e
              Nothing -> assertFailure "an inlined binder remains"
      [] -> assertFailure "no bindings"
  go _ _ = assertFailure "not a let-expression"

tests :: TestTree
tests =
  testGroup "Clash.Tests.Normalize.Scoped.Transformations.Inline"
    [ testGroup "regression tests" regressionTests
    , testGroup "inlineBndrsCleanup (#1337)"
        [ -- The bindings to inline form the cycle @a -> b -> c -> a@: @b@
          -- turns out to be recursive, and is kept
          testCase "t1337a" $
            inlineBndrsCleanup1337
              "let { (res :: Int) = b; (b :: Int) = b b } in res"
              [strQQ|
                let
                  -- Types don't mean anything for this example
                  { (res :: Int) = a
                  ; (a :: Int) = b
                  ; (b :: Int) = c
                  ; (c :: Int) = a b
                  } in res
              |]
          -- The bindings to inline form the cycle @a -> b -> c -> d -> a@:
          -- @c@ turns out to be recursive, and is kept
        , testCase "t1337b" $
            inlineBndrsCleanup1337
              "let { (res :: Int) = c; (c :: Int) = c c c } in res"
              [strQQ|
                let
                  { (res :: Int) = a
                  ; (a :: Int) = b
                  ; (b :: Int) = c
                  ; (c :: Int) = d
                  ; (d :: Int) = a b c
                  } in res
              |]
          -- Like @t1337a@, but a free variable of the let-expression is no
          -- let-binding to inline
        , testCase "t1337c" $
            inlineBndrsCleanup1337
              "let { (res :: Int) = b; (b :: Int) = b b (freevar_5 :: Int) } in res"
              [strQQ|
                let
                  { (res :: Int) = a
                  ; (a :: Int) = b
                  ; (b :: Int) = c
                  ; (c :: Int) = a b (freevar_5 :: Int)
                  } in res
              |]
        ]
    , testGroup "bindConstantVar"
        [ testCase "doesn't inline the function being normalized (#2839)" $
            keepsInTop mempty bindConstantVarS [] (tm "let { !(y :: Int) = top_G10 } in y")
        , testCase "doesn't inline another global, which isn't work-free-ish" $
            keepsInTop mempty bindConstantVarS [] (tm "let { !(y :: Int) = g_G11 } in y")
        , testCase "inlines a constant" $
            rewritesInTop mempty bindConstantVarS [] (tm "let { !(y :: Int) = 3 } in y") (tm "3")
        , testCase "inlines a local variable" $
            rewritesInTop mempty bindConstantVarS [varD "x_20" "Int"]
              (tm "let { !(y :: Int) = x_20 } in y") (tm "x_20")
        ]
    , testGroup "inlineCast"
        [ testCase "inlines a cast of a variable" $
            rewritesInTop mempty inlineCastS [varD "x_20" "Int"]
              (tm "let { !(y :: Int) = Core.cast @Int @Int x_20 } in fun_G12 y")
              (tm "fun_G12 (Core.cast @Int @Int x_20)")
        , testCase "inlines a cast of a global" $
            rewritesInTop mempty inlineCastS []
              (tm "let { !(y :: Int) = Core.cast @Int @Int g_G11 } in fun_G12 y")
              (tm "fun_G12 (Core.cast @Int @Int g_G11)")
        , testCase "doesn't inline a cast of an application" $
            keepsInTop mempty inlineCastS [varD "x_20" "Int"]
              (tm "let { !(y :: Int) = Core.cast @Int @Int (fun_G12 x_20) } in y")
        ]
    , testGroup "inlineSimIO"
        [ testCase "doesn't inline a binding of another type" $
            keepsInTop mempty inlineSimIOS [varD "x_20" "Int"]
              (tm "let { !(y :: Int) = fun_G12 x_20 } in y")
        , testCase "inlines a SimIO action at every occurrence" $
            rewritesInTop mempty inlineSimIOS [varD "x_20" "Int"]
              (tm "let { !(io :: Clash.Explicit.SimIO.SimIO_300 Int) = act_G13 x_20 } in both_G14 io io")
              (tm "both_G14 (act_G13 x_20) (act_G13 x_20)")
        ]
    , testGroup "collapseRHSNoops"
        [ -- E.g. the fake global of 'normalizeWorkFreeApp'
          testCase "doesn't fire in a function without a binding" $
            keepsInTop mempty collapseRHSNoopsS [varD "x_20" "Int"]
              (tm "let { !(y :: Int) = Prim.idPrim x_20 } in y")
        , testCase "collapses an identity primitive in an OPAQUE function" $
            rewritesInTop topOpaque collapseRHSNoopsS [varD "x_20" "Int"]
              (tm "let { !(y :: Int) = Prim.idPrim x_20 } in y")
              (tm "let { !(y :: Int) = GHC.Prim.unsafeCoerce# @Int @Int x_20 } in y")
        ]
    ]
 where
  bindConstantVarS = $(asScopedRewriteQ S.bindConstantVar)
  inlineCastS = $(asScopedRewriteQ S.inlineCast)
  inlineSimIOS = $(asScopedRewriteQ S.inlineSimIO)
  collapseRHSNoopsS = $(asScopedRewriteQ S.collapseRHSNoops)

-- | Regression tests for bugs in the inlining transformations
regressionTests :: [TestTree]
regressionTests =
  [ -- 'inlineCleanup' on a let-expression in which the binders that it wants
    -- to inline (data constructor applications bound to system names) form
    -- two nested cycles: @w -> y -> x -> w@ and @x <-> y@. Starting from @k@,
    -- it visits @w@, @y@ and @x@ in that order. So @x@ refers to two
    -- let-bindings that are still being processed. It used to inline @x@
    -- anyway, and only mark @y@ as recursive. Then it inlined @w@ into @k@ and
    -- dropped it, while the copy of @x@ in @k@ still referred to it.
    testCase "inlineCleanup keeps binders that inlined binders refer to" $
      assertRewritesTo env def $(asScopedRewriteQ S.inlineCleanup) []
        (tm [strQQ|
          let
            { (w_S :: Pair) = MkPair y_S y_S
            ; (y_S :: Pair) = MkPair x_S x_S
            ; (x_S :: Pair) = MkPair w_S y_S
            ; (k :: Pair) = MkPair w_S x_S
            } in k
        |])
        (tm [strQQ|
          let
            { (k :: Pair) = MkPair (MkPair (MkPair x_S x_S) (MkPair x_S x_S)) x_S
            ; (x_S :: Pair) = MkPair (MkPair (MkPair x_S x_S) (MkPair x_S x_S)) (MkPair x_S x_S)
            } in k
        |])

    -- A reference to a local variable which has the same unique as a global
    -- binder. Before #405, the inlining transformations didn't check whether
    -- the variable was local, and inlined the global binder instead.
    -- https://github.com/clash-lang/clash-compiler/pull/405
    -- https://github.com/clash-lang/clash-compiler/commit/16e65951c40b36ce64861819ff14e1a5b6cc0486
  , testCase "inlineSmall does not inline a local variable (#405)" $
      assertNoRewrite env fState $(asScopedRewriteQ S.inlineSmall) [varD "f_7" "Int"] (tm "f_7")
  , testCase "inlineWorkFree does not inline a local variable (#405)" $
      assertNoRewrite env fState $(asScopedRewriteQ S.inlineWorkFree) [varD "f_7" "Int"] (tm "f_7")

    -- 'collapseRHSNoops' on @let r = mapPrim f xs in r@, in a function in
    -- which the global no-op @f = \\x -> x@ is defined
  , testCase "collapseRHSNoops collapses a global no-op" $
      rewritesInTop (topOpaque <> fIdentity) collapseRHSNoopsS [varD "xs_11" "Int"]
        (tm "let { !(r :: Int) = Prim.mapPrim f_G7 xs_11 } in r")
        (tm "let { !(r :: Int) = GHC.Prim.unsafeCoerce# @Int @Int xs_11 } in r")

    -- A local variable @f@ with the same unique as the global no-op @f@ is
    -- not a no-op. 'collapseRHSNoops' used to look up @f@ in the global
    -- binders, and turned the let-binding into @r = unsafeCoerce# xs@.
  , testCase "collapseRHSNoops does not treat a local variable as a no-op" $
      keepsInTop (topOpaque <> fIdentity) collapseRHSNoopsS [varD "f_7" "Int -> Int", varD "xs_11" "Int"]
        (tm "let { !(r :: Int) = Prim.mapPrim f_7 xs_11 } in r")
  ]
 where
  collapseRHSNoopsS = $(asScopedRewriteQ S.collapseRHSNoops)
  -- A state with the binding @f = 42@ of the global @f :: Int@, with the same
  -- unique as the local variable @f@
  fState = def {_bindings = mkGlobalBindings [(testGlobalId "f_G7" "Int", "42")]}
  -- The binding of the global no-op @f = \\x -> x@
  fIdentity = mkGlobalBindings [(fGlobal, "\\(x :: Int) -> x")]
