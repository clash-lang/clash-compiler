{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.EtaExpand".
  'assertRewritesTo' runs every test in the scope of the free variables of its
  input, and in a scope that the binders of the input clash with.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.EtaExpand (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Syntax (GlobalId)
import qualified Clash.Normalize.Transformations.EtaExpand as Scoped
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite (Rewrite, Transform (..))
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteEnv, RewriteState)

import Test.Clash.Rewrite (TestDefs (..), TestTerm, parseTestTermWith, testGlobalId)
import Test.Clash.Core
  ( ScopeDecl, Step (..), assertNoRewriteAt, assertRewritesTo, varD, withTopEntities )

-- | Global functions @f, g :: Int -> Int@ and @k :: Int -> Int -> Int@
f, g, k :: GlobalId
f = testGlobalId "f_G10" "Int -> Int"
g = testGlobalId "g_G41" "Int -> Int"
k = testGlobalId "k_G42" "Int -> Int -> Int"

tm :: String -> TestTerm
tm = parseTestTermWith (TestDefs [] [] [f, g, k])

st :: RewriteState NormalizeState
st = def

etaTLS, etaExpandSynS :: Rewrite NormalizeState
etaTLS = Transform Scoped.etaExpansionTL
etaExpandSynS = $(asRewriteQ Scoped.etaExpandSyn)

-- | An environment in which @f@ and @k@ are top entities
topsEnv :: RewriteEnv
topsEnv = withTopEntities [f, k] def

-- | Assert that 'Scoped.etaExpansionTL' rewrites a term to the expected one
etaTLTo :: [ScopeDecl] -> String -> String -> Assertion
etaTLTo decls input expected = assertRewritesTo def st etaTLS decls (tm input) (tm expected)

-- | Assert that 'Scoped.etaExpandSyn' rewrites a term to the expected one
etaSynTo :: [ScopeDecl] -> String -> String -> Assertion
etaSynTo decls input expected = assertRewritesTo topsEnv st etaExpandSynS decls (tm input) (tm expected)

-- | Assert that a transformation, at the given path, leaves a term as it is
keepsAt :: RewriteEnv -> Rewrite NormalizeState -> [Step] -> [ScopeDecl] -> String -> Assertion
keepsAt env rw path decls = assertNoRewriteAt env st path rw decls . tm

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Transformations.EtaExpand"
  [ testGroup "regression tests"
      [ -- 'etaExpansionTL' moves the lambdas in the body of a let-expression
        -- out of that let-expression:
        --
        -- > let i = x in \y -> e  ==>  \y -> let i = x in e
        --
        -- It used to do so without renaming @y@. If @y@ has the same unique
        -- as @i@, the let-binder captures the references to @y@ in @e@.
        testCase "etaExpansionTL: let-binder does not capture lambda" $
          etaTLTo []
            "let { (i :: Int) = 3 } in \\(i :: Int) -> i"
            "\\(y :: Int) -> let { (i :: Int) = 3 } in y"
        -- Like the test above, but now the moved lambda captures a free
        -- variable of the right-hand side of the let-binding: if @y@ is free in
        -- @x@, then @\\y -> let i = x in e@ binds it.
      , testCase "etaExpansionTL: lambda does not capture let-binding" $
          etaTLTo []
            "\\(y :: Int) -> let { (i :: Int) = y } in \\(y :: Int) -> i"
            "\\(y :: Int) (y2 :: Int) -> let { (i :: Int) = y } in i"
        -- 'etaExpandSyn' eta-expands references to top entities. It shares
        -- 'topEntities' with
        -- 'Clash.Normalize.Transformations.Specialize.specialize',
        -- and also looked up whether a variable is a top entity by its unique
        -- alone, so it eta-expanded a local that shares its unique with a top
        -- entity.
      , testCase "etaExpandSyn does not expand a local that shares a unique with a top entity" $ do
          assertRewritesTo (withTopEntities [f] def) st etaExpandSynS []
            (tm "f_G10") (tm "\\(a :: Int) -> f_G10 a")
          keepsAt (withTopEntities [f] def) etaExpandSynS [] [varD "f_10" "Int -> Int"] "f_10"
      ]
  , testGroup "etaExpansionTL"
      [ testCase "eta-expands a function" $
          etaTLTo [] "k_G42" "\\(a :: Int) (b :: Int) -> k_G42 a b"
      , testCase "goes under lambdas" $
          etaTLTo [] "\\(x :: Int) -> k_G42 x" "\\(x :: Int) (a :: Int) -> k_G42 x a"
      , testCase "moves lambdas out of a let" $
          etaTLTo []
            "let { (i :: Int) = 3 } in \\(y :: Int) (z :: Int) -> i"
            "\\(y :: Int) (z :: Int) -> let { (i :: Int) = 3 } in i"
      , testCase "moves lambdas out of nested lets" $
          etaTLTo [varD "x_31" "Int"]
            "let { !(y :: Int) = x_31 } in \\(z :: Int) -> let { (x :: Int) = g_G41 y } in \\(y :: Int) -> x"
            "\\(z :: Int) (y :: Int) -> let { !(y2 :: Int) = x_31 } in let { (x :: Int) = g_G41 y2 } in x"
      , testCase "eta-expands the body of a let" $
          etaTLTo []
            "let { (y :: Int) = 3; (z :: Int) = g_G41 y } in k_G42 z"
            "\\(a :: Int) -> let { (y :: Int) = 3; (z :: Int) = g_G41 y } in k_G42 z a"
      , testCase "the body of a let is a local function" $
          etaTLTo [varD "h_43" "Int -> Int"]
            "let { !(y :: Int) = 3 } in h_43"
            "\\(a :: Int) -> let { !(y :: Int) = 3 } in h_43 a"
      , testCase "doesn't fire on a value" $
          keepsAt def etaTLS [] [varD "x_31" "Int"] "g_G41 x_31"
      ]
  , testGroup "etaExpandSyn"
      [ testCase "eta-expands a top entity" $
          etaSynTo [] "f_G10" "\\(a :: Int) -> f_G10 a"
      , testCase "eta-expands a partially applied top entity" $
          etaSynTo [varD "x_31" "Int"] "k_G42 x_31" "\\(a :: Int) -> k_G42 x_31 a"
      , testCase "looks through ticks" $
          etaSynTo [varD "x_31" "Int"]
            "Core.prefixName @Pfx (k_G42 x_31)"
            "\\(a :: Int) -> Core.prefixName @Pfx (k_G42 x_31) a"
      , testCase "doesn't eta-expand the function of an application" $
          keepsAt topsEnv etaExpandSynS [AppFunStep] [varD "x_31" "Int"] "k_G42 x_31"
      , testCase "doesn't eta-expand under ticks in the function of an application" $
          keepsAt topsEnv etaExpandSynS [AppFunStep, TickStep] [varD "x_31" "Int"]
            "(Core.prefixName @Pfx k_G42) x_31"
      , testCase "doesn't eta-expand a fully applied top entity" $
          keepsAt topsEnv etaExpandSynS [] [varD "x_31" "Int", varD "y_32" "Int"] "k_G42 x_31 y_32"
      , testCase "doesn't eta-expand a function that isn't a top entity" $
          keepsAt topsEnv etaExpandSynS [] [] "g_G41"
      ]
  ]
