{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize". The regression tests of 'cleanupGraph' build
  their binders in scoped Core, and assert that the result only mentions the
  variables it binds, and is the expected term.

  Note that there is no test for #448 ("Use proper InScopeSet in
  flattenCallTree"): 'flattenCallTree' substitutes globals only, whose
  replacements are closed terms, so they can't capture variables.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Normalize (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Driver.Types (Binding (..))
import Clash.Normalize (cleanupGraph)
import Clash.Rewrite.Types (RewriteState (..))

import Clash.Core.Scoped.NameMap (lookupGlobalEnv)
import Clash.Core.Scoped.Syntax (GlobalId)

import Test.Clash.Rewrite
  ( TestDefs (..), parseTestTermWith, runRewriteTest, testGlobalId )
import Test.Clash.Scoped
  (assertLintClean, assertTermIs, emptyTestScope, mkGlobalBindings)

-- | Run 'cleanupGraph' with the given top entity and (normalized) binders,
-- and assert that it gives the expected term for the top entity
cleanupGives :: [GlobalId] -> GlobalId -> [(GlobalId, String)] -> String -> Assertion
cleanupGives globals top bndrs expected = do
  let tm = parseTestTermWith (TestDefs [] [] globals)
      bm = mkGlobalBindings [(i, tm t) | (i, t) <- bndrs]
  (bm1, _, _) <- runRewriteTest def def{_bindings = bm} (cleanupGraph top bm)
  case lookupGlobalEnv top bm1 of
    Just b -> do
      assertLintClean emptyTestScope (bindingTerm b)
      assertTermIs emptyTestScope (tm expected) (bindingTerm b)
    Nothing -> assertFailure "cleanupGraph dropped the top entity"

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize"
  [ -- #445: when inlining a wrapper @f@ into its caller, 'flattenNode' strips
    -- the arguments @f@ passes on to the function it wraps. It did not check
    -- that the result binder of @f@ was not used in that application, so
    -- @f = \x y -> let r0 = g r0 x y in r0@ became @g r0@, with @r0@ free.
    -- https://github.com/clash-lang/clash-compiler/pull/445
    testCase "flattenNode keeps LHS of recursive binders (#445)" $ do
      let f = testGlobalId "f_G" "Int -> Int -> Int"
          g = testGlobalId "g_G" "Int -> Int -> Int -> Int"
          h = testGlobalId "h_G" "Int -> Int -> Int"
      cleanupGives [f, g, h] h
        [ (h, "\\(a :: Int) (b :: Int) -> let { (r1 :: Int) = f_G a b } in r1")
        , (f, "\\(x :: Int) (y :: Int) -> let { (r0 :: Int) = g_G r0 x y } in r0") ]
        "\\(a :: Int) (b :: Int) -> let { (r0 :: Int) = g_G r0 a b } in r0"

    -- 'stripArgs' used to only check whether the remaining (unstripped)
    -- arguments were /equal/ to one of the lambda-bound variables, not whether
    -- they /mentioned/ one. So @f = \x -> let r = g (p x) x in r@ became
    -- @g (p x)@, with @x@ free. Such non-ANF arguments occur for higher-order
    -- primitives.
    -- https://github.com/clash-lang/clash-compiler/commit/bb1ea69be69d5497bb9fb01a287adb607844a70d
  , testCase "flattenNode doesn't strip args when they are mentioned" $ do
      let f = testGlobalId "f_G" "Int -> Int"
          g = testGlobalId "g_G" "Int -> Int -> Int"
          h = testGlobalId "h_G" "Int -> Int"
          p = testGlobalId "p_G" "Int -> Int"
      cleanupGives [f, g, h, p] h
        [ (h, "\\(a :: Int) -> let { (r1 :: Int) = f_G a } in r1")
        , (f, "\\(x :: Int) -> let { (r0 :: Int) = g_G (p_G x) x } in r0") ]
        "\\(a :: Int) -> let { !(r_S :: Int) = g_G (p_G a) a } in r_S"
  ]
