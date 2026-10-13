{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Cast". 'assertRewritesTo'
  runs every test in the scope of the free variables of its input, and in a
  scope that the binders of the input clash with.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.Cast (tests) where

import Control.Exception (try)
import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.DataCon as C
import Clash.Core.Scoped.Syntax (GlobalId, Term (Cast))
import qualified Clash.Normalize.Scoped.Transformations.Cast as Scoped
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Scoped (Rewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteState)
import Clash.Util (ClashException)

import Test.Clash.Rewrite
  ( TestDefs (..), TestTerm, kindedTyVarD, parseTestTermWith, strQQ, testDataCon
  , testGlobalId )
import Test.Clash.Scoped
  ( ScopeDecl, assertCaptureRewritesTo, assertNoRewrite, assertRewritesTo, runScopedIn, termIn
  , typeIn, varD, withCurFun, withTestScope )

-- * Definitions

-- | @MkE :: forall b. b -> E@, a data constructor with an existential type
-- variable
mkE :: C.DataCon
mkE = testDataCon "MkE" 1001 1 [] [kindedTyVarD "b" "Type"] ["b"] "E"

-- | Global functions @f, g :: Int -> Int@
globals :: [GlobalId]
globals =
  [ testGlobalId "g_G2" "Int -> Int"
  , testGlobalId "f_G40" "Int -> Int"
  , testGlobalId "g_G41" "Int -> Int" ]

tm :: String -> TestTerm
tm = parseTestTermWith (TestDefs [mkE] [] globals)

st :: RewriteState NormalizeState
st = def

caseCastS, elimCastCastS, letCastS, splitCastWorkS :: Rewrite NormalizeState
caseCastS = $(asRewriteQ Scoped.caseCast)
elimCastCastS = $(asRewriteQ Scoped.elimCastCast)
letCastS = $(asRewriteQ Scoped.letCast)
splitCastWorkS = $(asRewriteQ Scoped.splitCastWork)

-- | Assert that a transformation rewrites a term to the expected one, see
-- 'assertRewritesTo'
rewrites :: Rewrite NormalizeState -> [ScopeDecl] -> TestTerm -> TestTerm -> Assertion
rewrites = assertRewritesTo def st

-- | Assert that a transformation leaves a term as it is
keeps :: Rewrite NormalizeState -> [ScopeDecl] -> TestTerm -> Assertion
keeps = assertNoRewrite def st

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.Cast"
  [ testGroup "regression tests"
      [ -- @let x = cast (g x) in x@, with the outer @x@ on the right-hand
        -- side: 'splitCastWork' must keep the binding non-recursive. It used to
        -- rebuild it as @letrec x' = g x; x = cast x' in x@, where @g x@ refers
        -- to the cast itself.
        testCase "splitCastWork keeps a NonRec let non-recursive" $
          rewrites splitCastWorkS [varD "x_0" "Int"]
            (tm "let { !(x :: Int) = Core.cast @Int @Int (g_G2 x_0) } in x")
            (tm "let { !(cx_I :: Int) = g_G2 x_0; !(x :: Int) = Core.cast @Int @Int cx_I } in x")
      ]
  , testGroup "caseCast"
      [ testCase "pushes a cast into the alternatives" $
          rewrites caseCastS [varD "s_30" "Pair"]
            (tm [strQQ|
              Core.cast @Int @(Int -> Int) ((case s_30 of { MkPair (x :: Int) (y :: Int) -> f_G40 x; _ -> 3 }) :: Int)
            |])
            (tm [strQQ|
              (case s_30 of
                { MkPair (x :: Int) (y :: Int) -> Core.cast @Int @(Int -> Int) (f_G40 x)
                ; _ -> Core.cast @Int @(Int -> Int) 3
                }) :: Int -> Int
            |])
      , testCase "looks through ticks" $
          rewrites caseCastS [varD "s_30" "Pair"]
            (tm [strQQ|
              Core.cast @Int @Int (Core.prefixName @Pfx ((case s_30 of { MkPair (x :: Int) (y :: Int) -> y }) :: Int))
            |])
            (tm "(case s_30 of { MkPair (x :: Int) (y :: Int) -> Core.cast @Int @Int y }) :: Int")
      , testCase "existential type variables that shadow the cast's" $ do
          rewrites caseCastS [kindedTyVarD "a_50" "Type", varD "s_30" "E", varD "y_32" "a_50"]
            (tm [strQQ|
              Core.cast @a_50 @Int ((case s_30 of { MkE (a'_51 :: Type) (x :: a'_51) -> y_32 }) :: a_50)
            |])
            (tm "(case s_30 of { MkE (a'_51 :: Type) (x :: a'_51) -> Core.cast @a_50 @Int y_32 }) :: Int")
          -- The existential type variable @a'@ has the name of the type
          -- variable @a@ of the cast
          assertCaptureRewritesTo def st caseCastS [varD "s_30" "E", varD "y_32" "Int"]
            (tm "(case s_30 of { MkE (a' :: Type) (x :: a') -> y_32 }) :: Int")
            [kindedTyVarD "a_50" "Type"]
            (\ts e -> Cast e (typeIn ts "Int") (typeIn ts "a_50"))
            (tm "(case s_30 of { MkE (a' :: Type) (x :: a') -> Core.cast @Int @a_50 y_32 }) :: a_50")
      , testCase "doesn't fire on another term" $
          keeps caseCastS [varD "x_31" "Int"]
            (tm "Core.cast @Int @Int (f_G40 x_31)")
      ]
  , testGroup "letCast"
      [ testCase "pushes a cast into the body of a let" $
          rewrites letCastS [varD "x_31" "Int"]
            (tm "Core.cast @Int @Int (let { !(y :: Int) = f_G40 x_31 } in g_G41 y)")
            (tm "let { !(y :: Int) = f_G40 x_31 } in Core.cast @Int @Int (g_G41 y)")
      , testCase "pushes a cast into the body of a letrec" $
          rewrites letCastS []
            (tm "Core.cast @Int @Int (let { (y :: Int) = f_G40 z; (z :: Int) = g_G41 y } in y)")
            (tm "let { (y :: Int) = f_G40 z; (z :: Int) = g_G41 y } in Core.cast @Int @Int y")
      , testCase "the cast may mention variables the let shadows" $
          rewrites letCastS [kindedTyVarD "a_50" "Type", varD "x_31" "Int"]
            (tm "Core.cast @Int @a_50 (let { !(x :: Int) = f_G40 x_31 } in x)")
            (tm "let { !(x :: Int) = f_G40 x_31 } in Core.cast @Int @a_50 x")
      , testCase "doesn't fire on another term" $
          keeps letCastS [varD "x_31" "Int"]
            (tm "Core.cast @Int @Int x_31")
      ]
  , testGroup "elimCastCast"
      [ testCase "eliminates back to back casts" $
          rewrites elimCastCastS [kindedTyVarD "a_50" "Type", varD "x_31" "Int"]
            (tm "Core.cast @a_50 @Int (Core.cast @Int @a_50 x_31)")
            (tm "x_31")
      , testCase "looks through ticks" $
          rewrites elimCastCastS [kindedTyVarD "a_50" "Type", varD "x_31" "Int"]
            (tm "Core.cast @a_50 @Int (Core.prefixName @Pfx (Core.cast @Int @a_50 x_31))")
            (tm "x_31")
      , testCase "doesn't fire on another term" $
          keeps elimCastCastS [varD "x_31" "Int"]
            (tm "Core.cast @Int @Int x_31")
      , testCase "fails on casts whose types don't line up" $
          withTestScope [kindedTyVarD "a_50" "Type", varD "x_31" "Int"] $ \ts -> do
            let e = termIn ts (tm "Core.cast @Int @Int (Core.cast @Int @a_50 x_31)")
                st1 = withCurFun (testGlobalId "top_G60" "Int") st
            r <- try (runScopedIn def st1 ts elimCastCastS e)
            case r of
              Left (_ :: ClashException) -> pure ()
              Right _ -> assertFailure "no exception"
      ]
  , testGroup "splitCastWork"
      [ testCase "splits a NonRec binding" $
          rewrites splitCastWorkS [varD "x_31" "Int"]
            (tm "let { !(y :: Int) = Core.cast @Int @Int (f_G40 x_31) } in y")
            (tm "let { !(cy_I :: Int) = f_G40 x_31; !(y :: Int) = Core.cast @Int @Int cy_I } in y")
      , testCase "a NonRec binding that shadows a free variable of its cast" $
          rewrites splitCastWorkS [varD "x_31" "Int"]
            (tm "let { !(x :: Int) = Core.cast @Int @Int (f_G40 x_31) } in x")
            (tm "let { !(cx_I :: Int) = f_G40 x_31; !(x :: Int) = Core.cast @Int @Int cx_I } in x")
      , testCase "looks through ticks" $
          rewrites splitCastWorkS [varD "x_31" "Int"]
            (tm "let { !(y :: Int) = Core.prefixName @Pfx (Core.cast @Int @Int (f_G40 x_31)) } in y")
            (tm "let { !(cy_I :: Int) = f_G40 x_31; !(y :: Int) = Core.cast @Int @Int cy_I } in y")
      , testCase "splits the bindings of a letrec in place" $
          rewrites splitCastWorkS [varD "x_31" "Int"]
            (tm [strQQ|
              let
                { (y :: Int) = Core.cast @Int @Int (f_G40 z)
                ; (z :: Int) = g_G41 x_31
                ; (s :: Pair) = Core.cast @Int @Pair (g_G41 y)
                } in s
            |])
            (tm [strQQ|
              let
                { (cy_I :: Int) = f_G40 z
                ; (y :: Int) = Core.cast @Int @Int cy_I
                ; (z :: Int) = g_G41 x_31
                ; (cs_I :: Int) = g_G41 y
                ; (s :: Pair) = Core.cast @Int @Pair cs_I
                } in s
            |])
      , testCase "new binders are fresh with respect to the let's binders" $
          rewrites splitCastWorkS []
            (tm "let { (y :: Int) = 3; (x :: Int) = Core.cast @Int @Int (g_G41 y) } in x")
            (tm [strQQ|
              let { (y :: Int) = 3; (cx_I :: Int) = g_G41 y; (x :: Int) = Core.cast @Int @Int cx_I } in x
            |])
      , testCase "doesn't split a cast of a variable" $
          keeps splitCastWorkS [varD "x_31" "Int"]
            (tm [strQQ|
              let { (y :: Int) = Core.cast @Int @Int x_31; (z :: Int) = Core.cast @Int @Int f_G40 } in y
            |])
      , testCase "doesn't split a cast of a cast" $
          keeps splitCastWorkS [varD "x_31" "Int"]
            (tm "let { !(y :: Int) = Core.cast @Int @Int (Core.cast @Int @Int (f_G40 x_31)) } in y")
      ]
  ]
