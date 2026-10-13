{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Netlist.Scoped.Util"
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Netlist.Scoped.Util (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Scoped.Syntax (Term (..), tmNameRaw)
import Clash.Netlist.Scoped.Types (LocalId (..))
import Clash.Netlist.Scoped.Util (SplitNormalized (..), splitNormalized)

import Test.Clash.Rewrite (TestDefs (..), TestTerm, parseTestTermWith)
import Test.Clash.Scoped (closedTerm, testGlobalId, testTypeEnv)

-- | A term with the globals @g@ and @h@ of type @Int -> Int@
withGlobals :: String -> TestTerm
withGlobals =
  parseTestTermWith
    (TestDefs [] [] [testGlobalId "g_G10" "Int -> Int", testGlobalId "h_G11" "Int -> Int"])

-- | The raw name of a local variable
raw :: LocalId n -> Int
raw = tmNameRaw . localIdName

-- | The raw name of the argument of an application of a global, if it is a
-- local variable
argRaw :: Term n -> Maybe Int
argRaw = \case
  App (GlobalVar _) (Var _ _ n) -> Just (tmNameRaw n)
  _ -> Nothing

-- | The binder of a non-recursive let-binding is not in scope of its
-- right-hand side, which here refers to the argument @x@:
--
-- @
-- \\x -> let x = g x in x
-- @
--
-- Merging that binding into the bindings of the netlist as is would turn it
-- into the loop @x = g x@.
splitNormalizedNonRec :: Assertion
splitNormalizedNonRec =
  case splitNormalized testTypeEnv (closedTerm (withGlobals "\\(x :: Int) -> let { !(x :: Int) = g_G10 x } in x")) of
    Right (SplitNormalized _ [x] [(x', rhs)] res) -> do
      assertBool "the let-binder is not the argument" (raw x' /= raw x)
      argRaw rhs @?= Just (raw x)
      raw res @?= raw x'
    Right _ -> assertFailure "unexpected split"
    Left err -> assertFailure err

-- | Like 'splitNormalizedNonRec', but for a nested let-expression, whose
-- binding refers to a binding of the let-expression it is nested in:
--
-- @
-- \\a -> letrec y = h a in let y = g y in y
-- @
--
-- Merging the nested let-expression as is would give @letrec y = h a; y = g y
-- in y@, binding @y@ twice.
splitNormalizedNestedNonRec :: Assertion
splitNormalizedNestedNonRec =
  case splitNormalized testTypeEnv (closedTerm (withGlobals "\\(a :: Int) -> let { (y :: Int) = h_G11 a } in let { !(y :: Int) = g_G10 y } in y")) of
    Right (SplitNormalized _ [a] [(y, rhsY), (y', rhsY')] res) -> do
      assertBool "the let-binders differ" (raw y /= raw y')
      argRaw rhsY @?= Just (raw a)
      argRaw rhsY' @?= Just (raw y)
      raw res @?= raw y'
    Right _ -> assertFailure "unexpected split"
    Left err -> assertFailure err

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Netlist.Scoped.Util"
    [ testCase "splitNormalized keeps a NonRec binder apart from a variable its right-hand side refers to"
        splitNormalizedNonRec
    , testCase "splitNormalized keeps a nested NonRec binder apart from a binder its right-hand side refers to"
        splitNormalizedNestedNonRec
    ]
