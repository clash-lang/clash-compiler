{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Netlist.Util"
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Netlist.Util (tests) where

import Control.Monad.State.Strict (runState)
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Annotations.BitRepresentation.Internal (buildCustomReprs)
import Clash.Core.Name (nameOcc)
import Clash.Core.Scoped.Syntax (Distinct, Scope, Term (..), Type, emptyScope, tmNameRaw)
import Clash.Core.Scoped.Type (TypeView (..), tyView)
import Clash.Netlist.Types
  (FilteredHWType (..), HWMap, HWType (..), HWTypeTranslator (..), LocalId (..))
import Clash.Netlist.Util (SplitNormalized (..), coreTypeToHWType, splitNormalized)

import Test.Clash.Rewrite (TestDefs (..), TestTerm, parseTestTermWith)
import Test.Clash.Scoped
  ( TestScope (..), closedTerm, closedType, testGlobalId, testTypeEnv, tyVarD, typeIn
  , withTestScope )

-- | Translates @Int@ to a 64-bit signed number, and nothing else
intTranslator :: HWTypeTranslator
intTranslator = HWTypeTranslator $ \_reprs _te _sc ty -> pure $ case tyView ty of
  TyConApp tc [] | nameOcc tc == "Int" -> Just (Right (FilteredHWType (Signed 64) []))
  _ -> Nothing

-- | Translate types of a scope in turn, starting from an empty cache. The
-- type constructors are those of 'Test.Clash.Rewrite.testTyConMap', among
-- which @E@ with its constructor @MkE :: forall b. b -> E@.
translate
  :: Distinct n => Scope n -> [Type n] -> ([Either String FilteredHWType], HWMap)
translate sc tys = flip runState mempty $
  mapM (coreTypeToHWType intTranslator (buildCustomReprs []) testTypeEnv sc) tys

translationTests :: [TestTree]
translationTests =
  [ testCase "an algebraic data type is a sum of products" $
      case translate emptyScope [closedType "Maybe Int"] of
        ([Right (FilteredHWType (SP _ [(_, []), (_, [Signed 64])]) _)], _) -> pure ()
        r -> assertFailure ("unexpected translation: " <> show (fst r))

  , testCase "the field types of a constructor with an existential are not cached" $
      -- The field of MkE has the type of the existentially quantified type
      -- variable, which can't be translated
      case translate emptyScope [closedType "E"] of
        ([Left _], cache) -> Map.size cache @?= 1
        r -> assertFailure ("unexpected translation: " <> show (fst r))

  , testCase "open types are not cached" $
      withTestScope [tyVarD "a_70"] $ \ts ->
        case translate (testScope ts) [typeIn ts "Maybe a_70"] of
          ([Left _], cache) -> Map.size cache @?= 0
          r -> assertFailure ("unexpected translation: " <> show (fst r))

  , testCase "alpha-equivalent types share their cache entry" $
      case translate emptyScope [closedType "forall a. Maybe a", closedType "forall b. Maybe b"] of
        ([Left _, Left _], cache) -> Map.size cache @?= 1
        r -> assertFailure ("unexpected translation: " <> show (fst r))
  ]

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
    "Clash.Tests.Netlist.Util"
    [ testGroup "coreTypeToHWType" translationTests
    , testCase "splitNormalized keeps a NonRec binder apart from a variable its right-hand side refers to"
        splitNormalizedNonRec
    , testCase "splitNormalized keeps a nested NonRec binder apart from a binder its right-hand side refers to"
        splitNormalizedNestedNonRec
    ]
