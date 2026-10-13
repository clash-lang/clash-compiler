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
import Clash.Core.Name (NameSort (User), nameOcc)
import Clash.Core.Term (Bind (..), Term (..), mkApps, mkLams)
import qualified Clash.Core.Type as C
import Clash.Core.Scoped.Type (TypeView (..), tyView)
import Clash.Core.Scoped.TypeEnv (TypeEnv, mkTypeEnv)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Netlist.Types
  (FilteredHWType (..), HWMap, HWType (..), HWTypeTranslator (..))
import Clash.Netlist.Util (coreTypeToHWType, splitNormalized, withLegacyType)
import Clash.Unique (Unique)

import Test.Clash.Rewrite
  ( assertAlphaEq, existentialDataCon, existentialTy, globalId, intFunTy, intId
  , intTy, intTyCon, maybeTy, fxAlgTyCon, parseTyConName, testTyConMap, tyVar )

-- | Apply the global function with the given name and unique to the given
-- arguments
app :: String -> Unique -> [Term] -> Term
app f u args = mkApps (Var (globalId User f u (intFunTy (length args)))) (map Left args)

-- | Run 'splitNormalized', and rebuild its result as a term in which all
-- let-bindings form a single recursive group, as the netlist treats them
splitNormalizedRebuild :: Term -> IO Term
splitNormalizedRebuild e = case splitNormalized mempty e of
  Right (args, binds, res) -> pure (mkLams (Letrec binds (Var res)) args)
  Left err -> assertFailure err

-- | The binder of a non-recursive let-binding is not in scope of its
-- right-hand side, which here refers to the argument @x@:
--
-- @
-- \\x -> let x = g x in x
-- @
--
-- Merging that binding into the bindings of the netlist as is, turns it into
-- the loop @x = g x@.
splitNormalizedNonRec :: Assertion
splitNormalizedNonRec = do
  actual <- splitNormalizedRebuild
    (Lam x (Let (NonRec x (app "g" 10 [Var x])) (Var x)))
  assertAlphaEq
    (Lam x (Letrec [(x', app "g" 10 [Var x])] (Var x')))
    actual
 where
  x = intId User "x" 0
  x' = intId User "x'" 1

-- | Like 'splitNormalizedNonRec', but for a nested let-expression, whose
-- binding refers to a binding of the let-expression it is nested in:
--
-- @
-- \\a -> letrec y = h a in let y = g y in y
-- @
--
-- Merging the nested let-expression as is gives @letrec y = h a; y = g y in
-- y@, binding @y@ twice.
splitNormalizedNestedNonRec :: Assertion
splitNormalizedNestedNonRec = do
  actual <- splitNormalizedRebuild
    (Lam a (Letrec [(y, app "h" 10 [Var a])]
             (Let (NonRec y (app "g" 11 [Var y])) (Var y))))
  assertAlphaEq
    (Lam a (Letrec [(y, app "h" 10 [Var a]), (y', app "g" 11 [Var y])] (Var y')))
    actual
 where
  a = intId User "a" 0
  y = intId User "y" 1
  y' = intId User "y'" 2

-- | Translates @Int@ to a 64-bit signed number, and nothing else
intTranslator :: HWTypeTranslator
intTranslator = HWTypeTranslator $ \_reprs _te _sc ty -> pure $ case tyView ty of
  TyConApp tc [] | nameOcc tc == "Int" -> Just (Right (FilteredHWType (Signed 64) []))
  _ -> Nothing

-- | The test types, and @E@ with its constructor @MkE :: forall b. b -> E@
translationTypeEnv :: TypeEnv
translationTypeEnv = mkTypeEnv $
  UniqMap.insert (parseTyConName "E") (fxAlgTyCon (parseTyConName "E") [] [existentialDataCon]) $
  UniqMap.insert (parseTyConName "Int") intTyCon testTyConMap

-- | Translate types of Clash Core in turn, starting from an empty cache
translate :: [C.Type] -> ([Either String FilteredHWType], HWMap)
translate tys = flip runState mempty $
  mapM (\ty -> withLegacyType ty (coreTypeToHWType intTranslator (buildCustomReprs []) translationTypeEnv)) tys

translationTests :: [TestTree]
translationTests =
  [ testCase "an algebraic data type is a sum of products" $
      case translate [maybeTy intTy] of
        ([Right (FilteredHWType (SP _ [(_, []), (_, [Signed 64])]) _)], _) -> pure ()
        r -> assertFailure ("unexpected translation: " <> show (fst r))

  , testCase "the field types of a constructor with an existential are not cached" $
      -- The field of MkE has the type of the existentially quantified type
      -- variable, which can't be translated
      case translate [existentialTy] of
        ([Left _], cache) -> Map.size cache @?= 1
        r -> assertFailure ("unexpected translation: " <> show (fst r))

  , testCase "open types are not cached" $
      let a = tyVar User "a" 70
      in case translate [maybeTy (C.VarTy a)] of
           ([Left _], cache) -> Map.size cache @?= 0
           r -> assertFailure ("unexpected translation: " <> show (fst r))

  , testCase "alpha-equivalent types share their cache entry" $
      let a = tyVar User "a" 70
          b = tyVar User "b" 71
      in case translate [C.ForAllTy a (maybeTy (C.VarTy a)), C.ForAllTy b (maybeTy (C.VarTy b))] of
           ([Left _, Left _], cache) -> Map.size cache @?= 1
           r -> assertFailure ("unexpected translation: " <> show (fst r))
  ]

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Netlist.Util"
    [ testCase "splitNormalized renames a NonRec binder its right-hand side refers to"
        splitNormalizedNonRec
    , testCase "splitNormalized renames a nested NonRec binder its right-hand side refers to"
        splitNormalizedNestedNonRec
    , testGroup "coreTypeToHWType" translationTests
    ]
