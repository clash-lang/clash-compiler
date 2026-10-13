{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Netlist.Util"
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Netlist.Util (tests) where

import Control.Monad.State.Strict (runState)
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Annotations.BitRepresentation.Internal (buildCustomReprs)
import Clash.Core.Name (nameOcc)
import Clash.Core.Scoped.Syntax (Distinct, Scope, Type, emptyScope)
import Clash.Core.Scoped.Type (TypeView (..), tyView)
import Clash.Netlist.Types
  (FilteredHWType (..), HWMap, HWType (..), HWTypeTranslator (..))
import Clash.Netlist.Util (coreTypeToHWType)

import Test.Clash.Scoped
  (TestScope (..), closedType, testTypeEnv, tyVarD, typeIn, withTestScope)

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

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Netlist.Util"
    [ testGroup "coreTypeToHWType" translationTests
    ]
