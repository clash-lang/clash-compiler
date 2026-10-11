{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for data constructors in scoped Clash Core
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.DataCon (tests) where

import Data.Maybe (fromMaybe)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Literal as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Subst as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Util as C (dataConInstArgTys)
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.DataCon
import Clash.Core.Scoped.Internal.Scope (scopeWatermark)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TyCon (dataConSig)

import Test.Clash.Rewrite
  (intId, intTy, localId, mkDataCon, pairDataCon, pairTy, parseTyConTy, tyVar)
import Test.Clash.Scoped
  ( assertScopedAlphaEq, assertScopedTyAlphaEq, lintScopeOf, lintTerm, testTypeEnv
  , withFreeVars, withTests)
import Test.Clash.Scoped.Gen (DataConInst (..), genDataConInst, showType)

-- | Whether two lists of types are pairwise alpha-equivalent
aeqTypes :: [C.Type] -> [C.Type] -> Property
aeqTypes expected actual =
  counterexample
    ( "expected:\n" <> unlines (map showType expected) <>
      "but got:\n" <> unlines (map showType actual) )
    (length expected == length actual && and (zipWith C.aeqType expected actual))

-- | Check the existentials and field types that 'dataConInstExtTyVars' gives,
-- given the watermarks of the scopes it was given and gave
extTyVarsAgree :: C.DataCon -> [C.Type] -> Int -> Int -> [C.TyVar] -> [C.Type] -> Property
extTyVarsAgree dc univTys wm wm1 exts argTys =
  counterexample "existentials above the watermark" (all (> wm) raws) .&&.
    counterexample "existentials increasing" (and (zipWith (<) raws (drop 1 raws))) .&&.
    counterexample "scope watermark" (wm1 === maximum (wm : raws)) .&&.
    counterexample "kinds" (aeqTypes expectedKinds (map C.varType exts)) .&&.
    counterexample "field types" (aeqTypes expectedArgTys argTys)
 where
  raws = map (fromIntegral . C.varUniq) exts :: [Int]
  univ = C.dcUnivTyVars dc
  expectedKinds = map (C.substTyWith univ univTys . C.varType) (C.dcExtTyVars dc)
  -- The field types, with the existentials as arguments
  expectedArgTys = fromMaybe [] (C.dataConInstArgTys dc (univTys <> map C.VarTy exts))

-- | The raw names that a pattern binds
patRaws :: Pat n l -> [Int]
patRaws = \case
  DataPat _ tvs ids -> map (fromIntegral . C.varUniq) (teleTyVars tvs) <> idRaws ids
  _ -> []
 where
  idRaws :: Tele IdBinder x y -> [Int]
  idRaws TNil = []
  idRaws (TCons b bs) = tmNameRaw (tmNameOf b) : idRaws bs

-- | The type variables of a telescope, in Clash Core
teleTyVars :: Tele TyBinder n l -> [C.TyVar]
teleTyVars TNil = []
teleTyVars (TCons b bs) = toClashTyBinder b : teleTyVars bs

-- | @MkF :: forall a. forall b. a -> b -> F a@, with an existential type
-- variable @b@
fTyCon :: C.Type
fTyCon = parseTyConTy "F"

aTv, bTv :: C.TyVar
aTv = tyVar C.User "a" 50
bTv = tyVar C.User "b" 51

mkF :: C.DataCon
mkF = mkDataCon "MkF" 1003 [aTv] [bTv] [C.VarTy aTv, C.VarTy bTv] (C.AppTy fTyCon (C.VarTy aTv))

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.DataCon"
  [ testCase "dataConSig" $
      case dataConSig mkF of
        DataConSig (TCons ua TNil) (TCons eb TNil) [t1, t2] -> do
          -- The field types refer to the binders of the telescopes
          case (t1, t2) of
            (VarTy _ _ n1, VarTy _ _ n2) -> do
              tyNameRaw n1 @?= tyNameRaw (tyNameOf ua)
              tyNameRaw n2 @?= tyNameRaw (tyNameOf eb)
            _ -> assertFailure "unexpected field types"
        _ -> assertFailure "unexpected signature"

  , testCase "dataConInstArgTys" $ do
      let int = fromClashTypeClosed emptyScope intTy
      case dataConInstArgTys testTypeEnv emptyScope pairDataCon [] of
        Just [t1, t2] -> do
          assertScopedTyAlphaEq intTy t1
          assertScopedTyAlphaEq intTy t2
        _ -> assertFailure "unexpected result"
      case dataConInstArgTys testTypeEnv emptyScope mkF [int, int] of
        Just [t1, t2] -> do
          assertScopedTyAlphaEq intTy t1
          assertScopedTyAlphaEq intTy t2
        _ -> assertFailure "unexpected result"
      assertBool "too few types" (null (dataConInstArgTys testTypeEnv emptyScope mkF [int]))
      assertBool "too many types" (null (dataConInstArgTys testTypeEnv emptyScope mkF [int, int, int]))

    -- Instantiating @a := c@, where the free type variable @c@ has the same
    -- name as the existential @b@, must rename @b@.
  , testCase "instantiation renames existentials that would capture" $ do
      -- In the empty scope, a gets name 1 and b gets name 2
      let c = tyVar C.User "c" 2
      withConvEnv [c] [] $ \env -> do
        let r = dataConInstExtTyVars testTypeEnv (convScope env) mkF [fromClashType env (C.VarTy c)] $
                  \_ ext fieldTys -> case (ext, fieldTys) of
                    (TCons eb TNil, [VarTy _ _ n1, VarTy _ _ n2]) -> do
                      assertBool "renamed" (tyNameRaw (tyNameOf eb) /= 2)
                      tyNameRaw n1 @?= 2
                      tyNameRaw n2 @?= tyNameRaw (tyNameOf eb)
                    _ -> assertFailure "unexpected result"
        maybe (assertFailure "dataConInstExtTyVars failed") id r

  , testCase "withDataConPat" $ do
      let s = localId C.User "s" 1 pairTy
          names = [C.mkUnsafeSystemName "a" 0, C.mkUnsafeSystemName "b" 0]
      withConvEnv [] [s] $ \env -> case fromClashTerm env (C.Var s) of
        sVar -> do
          let sc = convScope env
              r = withDataConPat testTypeEnv sc pairDataCon [] names $ \_ p vars -> case vars of
                [va, _] -> Case sVar (fromClashTypeClosed sc intTy) [Alt p va]
                _ -> error "unexpected fields"
          case r of
            Just tm -> assertScopedAlphaEq
              (C.Case (C.Var s) intTy
                [(C.DataPat pairDataCon [] [intId C.User "a" 2, intId C.User "b" 3], C.Var (intId C.User "a" 2))])
              tm
            Nothing -> assertFailure "withDataConPat failed"
  , testCase "dataConType" $
      assertScopedTyAlphaEq (C.dcType mkF) (dataConType testTypeEnv emptyScope mkF)

  , withTests 1000 $ testGroup "properties"
      [ testProperty "dataConInstArgTys agrees with Clash" $
          forAll genDataConInst $ \(DataConInst fvs dc univTys extTys) ->
            withFreeVars fvs $ \env -> do
              let tys = univTys <> extTys
                  expected = C.dataConInstArgTys dc tys
                  actual = dataConInstArgTys testTypeEnv (convScope env) dc (map (fromClashType env) tys)
              case (expected, map toClashType <$> actual) of
                (Just e, Just a) -> aeqTypes e a
                (Nothing, Nothing) -> property True
                (e, a) -> counterexample (show (fmap (map showType) e, fmap (map showType) a)) False

        -- The existentials are bound by fresh binders, whose kinds are
        -- instantiated
      , testProperty "dataConInstExtTyVars agrees with Clash" $
          forAll genDataConInst $ \(DataConInst fvs dc univTys _) ->
            withFreeVars fvs $ \env -> do
              let sc = convScope env
                  univTys1 = map (fromClashType env) univTys
              fromMaybe (counterexample "Nothing" False) $
                dataConInstExtTyVars testTypeEnv sc dc univTys1 $ \sc1 ext argTys ->
                  extTyVarsAgree dc univTys (scopeWatermark sc) (scopeWatermark sc1)
                    (teleTyVars ext) (map toClashType argTys)

      , testProperty "withDataConPat binds fresh variables" $
          forAll genDataConInst $ \(DataConInst fvs dc univTys _) ->
            withFreeVars fvs $ \env -> do
              let sc = convScope env
                  wm = scopeWatermark sc
                  names = take (length (C.dcArgTys dc)) (map (C.mkUnsafeSystemName "f") [0 ..])
                  res = withDataConPat testTypeEnv sc dc (map (fromClashType env) univTys) names $ \sc1 p vars ->
                          let tm = Case (Literal (C.IntLiteral 0)) (fromClashTypeClosed sc intTy)
                                     [Alt p (foldl App (Data dc) vars)]
                              raws = patRaws p
                          in  counterexample "binders above the watermark" (all (> wm) raws) .&&.
                                counterexample "binders increasing" (and (zipWith (<) raws (drop 1 raws))) .&&.
                                counterexample "scope watermark" (scopeWatermark sc1 === maximum (wm : raws)) .&&.
                                lintTerm (lintScopeOf env fvs) tm === []
              fromMaybe (counterexample "Nothing" False) res
      ]
  ]
