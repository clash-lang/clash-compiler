{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for data constructors in scoped Clash Core
-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.DataCon (tests) where

import Data.Maybe (fromMaybe)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Literal as C
import qualified Clash.Core.Name as C

import Clash.Core.Alpha (aeqTy)
import Clash.Core.DataCon
import Clash.Core.Internal.Scope (scopeWatermark)
import Clash.Core.Syntax

import Test.Clash.Rewrite (pairDataCon)
import Test.Clash.Core
  ( TestScope (..), assertTermIs, assertTypeIs, atRaw, closedType, emptyTestScope, lintTerm
  , termIn, testLintScope, testTypeEnv, tyVarD, typeIn, varD, withTestScope, withTests)
import Test.Clash.Core.Gen
  (DataConInst (..), GenCon (..), fDataCon, genDataConInst, showTy, typeEnv)

-- | Whether two lists of types are pairwise alpha-equivalent
aeqTypes :: [Type n] -> [Type n] -> Property
aeqTypes expected actual =
  counterexample
    ( "expected:\n" <> unlines (map showTy expected) <>
      "but got:\n" <> unlines (map showTy actual) )
    (length expected == length actual && and (zipWith aeqTy expected actual))

-- | Check the existentials and field types that 'dataConInstExtTyVars' gives,
-- against the signature of the data constructor, given the watermarks of the
-- scopes it was given and gave
extTyVarsAgree
  :: DExt n l => GenCon -> [Type n] -> Int -> Int -> Tele TyBinder n l -> [Type l] -> Property
extTyVarsAgree gc univTys wm wm1 ext argTys =
  counterexample "existentials above the watermark" (all (> wm) raws) .&&.
    counterexample "existentials increasing" (and (zipWith (<) raws (drop 1 raws))) .&&.
    counterexample "scope watermark" (wm1 === maximum (wm : raws)) .&&.
    counterexample "kinds" (aeqTypes (gcExtKinds gc univTys) (teleKinds ext)) .&&.
    counterexample "field types" (aeqTypes (gcArgTys gc (map sink univTys) (teleVars ext)) argTys)
 where
  raws = teleTyRaws ext

-- | The kinds of the binders of a telescope of existentials, which bind
-- nothing their kinds mention
teleKinds :: Tele TyBinder n l -> [Type n]
teleKinds = \case
  TNil -> []
  TCons b TNil -> [tyBinderKind b]
  TCons {} -> error "teleKinds: more than one existential"

-- | The variables of a telescope of existentials
teleVars :: DExt n l => Tele TyBinder n l -> [Type l]
teleVars = \case
  TNil -> []
  TCons b TNil -> [tyBinderVar b]
  TCons {} -> error "teleVars: more than one existential"

teleTyRaws :: Tele TyBinder x y -> [Int]
teleTyRaws TNil = []
teleTyRaws (TCons b bs) = tyNameRaw (tyNameOf b) : teleTyRaws bs

-- | The raw names that a pattern binds
patRaws :: Pat n l -> [Int]
patRaws = \case
  DataPat _ tvs ids -> teleTyRaws tvs <> idRaws ids
  _ -> []
 where
  idRaws :: Tele IdBinder x y -> [Int]
  idRaws TNil = []
  idRaws (TCons b bs) = tmNameRaw (tmNameOf b) : idRaws bs

tests :: TestTree
tests = testGroup "Clash.Tests.Core.DataCon"
  [ testCase "dataConSig" $
      case C.dcSig fDataCon of
        DataConSig (TCons ua TNil) (TCons eb TNil) [t1, t2] -> do
          -- The field types refer to the binders of the telescopes
          case (t1, t2) of
            (VarTy _ _ n1, VarTy _ _ n2) -> do
              tyNameRaw n1 @?= tyNameRaw (tyNameOf ua)
              tyNameRaw n2 @?= tyNameRaw (tyNameOf eb)
            _ -> assertFailure "unexpected field types"
        _ -> assertFailure "unexpected signature"

  , testCase "dataConInstArgTys" $ do
      let int = closedType "Int"
      case dataConInstArgTys testTypeEnv emptyScope pairDataCon [] of
        Just [t1, t2] -> do
          assertTypeIs emptyTestScope "Int" t1
          assertTypeIs emptyTestScope "Int" t2
        _ -> assertFailure "unexpected result"
      case dataConInstArgTys typeEnv emptyScope fDataCon [int, int] of
        Just [t1, t2] -> do
          assertTypeIs emptyTestScope "Int" t1
          assertTypeIs emptyTestScope "Int" t2
        _ -> assertFailure "unexpected result"
      assertBool "too few types" (null (dataConInstArgTys typeEnv emptyScope fDataCon [int]))
      assertBool "too many types" (null (dataConInstArgTys typeEnv emptyScope fDataCon [int, int, int]))

    -- Instantiating @a := c@, where the free type variable @c@ has the same
    -- name as the existential @b@, must rename @b@.
  , testCase "instantiation renames existentials that would capture" $
      -- In the empty scope, a gets name 1 and b gets name 2
      withTestScope [atRaw 2 (tyVarD "c")] $ \ts -> do
        let r = dataConInstExtTyVars typeEnv (testScope ts) fDataCon [typeIn ts "c"] $
                  \_ ext fieldTys -> case (ext, fieldTys) of
                    (TCons eb TNil, [VarTy _ _ n1, VarTy _ _ n2]) -> do
                      assertBool "renamed" (tyNameRaw (tyNameOf eb) /= 2)
                      tyNameRaw n1 @?= 2
                      tyNameRaw n2 @?= tyNameRaw (tyNameOf eb)
                    _ -> assertFailure "unexpected result"
        maybe (assertFailure "dataConInstExtTyVars failed") id r

  , testCase "withDataConPat" $
      withTestScope [varD "s" "Pair"] $ \ts -> do
        let sc = testScope ts
            names = [C.mkUnsafeSystemName "a" 0, C.mkUnsafeSystemName "b" 0]
            r = withDataConPat testTypeEnv sc pairDataCon [] names $ \_ p vars -> case vars of
              [va, _] -> Case (termIn ts "s") (typeIn ts "Int") [Alt p va]
              _ -> error "unexpected fields"
        case r of
          Just tm -> assertTermIs ts "(case s of { MkPair (a :: Int) (b :: Int) -> a }) :: Int" tm
          Nothing -> assertFailure "withDataConPat failed"

  , testCase "dcType" $
      assertTypeIs emptyTestScope "forall (a :: Type) (b :: Type). a -> b -> GF a"
        (C.dcType fDataCon)

  , withTests 1000 $ testGroup "properties"
      [ testProperty "dataConInstArgTys agrees with the signature" $
          forAll genDataConInst $ \(DataConInst ts gc univTys extTys) -> do
            let expected
                  | length extTys == length (gcExtKinds gc univTys) = Just (gcArgTys gc univTys extTys)
                  | otherwise = Nothing
                actual = dataConInstArgTys typeEnv (testScope ts) (gcDataCon gc) (univTys <> extTys)
            case (expected, actual) of
              (Just e, Just a) -> aeqTypes e a
              (Nothing, Nothing) -> property True
              (e, a) -> counterexample (show (fmap (map showTy) e, fmap (map showTy) a)) False

        -- The existentials are bound by fresh binders, whose kinds are
        -- instantiated
      , testProperty "dataConInstExtTyVars agrees with the signature" $
          forAll genDataConInst $ \(DataConInst ts gc univTys _) -> do
            let sc = testScope ts
            fromMaybe (counterexample "Nothing" False) $
              dataConInstExtTyVars typeEnv sc (gcDataCon gc) univTys $ \sc1 ext argTys ->
                extTyVarsAgree gc univTys (scopeWatermark sc) (scopeWatermark sc1) ext argTys

      , testProperty "withDataConPat binds fresh variables" $
          forAll genDataConInst $ \(DataConInst ts gc univTys _) -> do
            let sc = testScope ts
                wm = scopeWatermark sc
                dc = gcDataCon gc
                nFields = length (gcArgTys gc univTys (gcExtKinds gc univTys))
                names = take nFields (map (C.mkUnsafeSystemName "f") [0 ..])
                res = withDataConPat typeEnv sc dc univTys names $ \sc1 p vars ->
                        let tm = Case (Literal (C.IntLiteral 0)) (typeIn ts "Int")
                                   [Alt p (foldl App (Data dc) vars)]
                            raws = patRaws p
                        in  counterexample "binders above the watermark" (all (> wm) raws) .&&.
                              counterexample "binders increasing" (and (zipWith (<) raws (drop 1 raws))) .&&.
                              counterexample "scope watermark" (scopeWatermark sc1 === maximum (wm : raws)) .&&.
                              lintTerm (testLintScope ts) tm === []
            fromMaybe (counterexample "Nothing" False) res
      ]
  ]
