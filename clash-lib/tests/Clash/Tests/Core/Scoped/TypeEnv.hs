{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the environment of the types of scoped Clash Core: its lookups
  agree with converting the signatures, and convert every signature only once.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.TypeEnv (tests) where

import Control.Monad (forM_)
import Data.Maybe (isJust)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (makeStableName)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Subst as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Scoped.Convert hiding (mkDataCon)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TyCon
import Clash.Core.Scoped.TypeEnv

import Test.Clash.Rewrite
  (assertErrorContains, intTy, mkDataCon, nTyConName, testTyConMap)
import Test.Clash.Scoped (assertScopedTyAlphaEq, testTypeEnv, withFreeVars, withTests)
import qualified Test.Clash.Scoped.Gen as Gen

-- | Whether two values are the same heap object, after evaluating them
sameObject :: a -> a -> Bool
sameObject a b = unsafePerformIO $ do
  na <- makeStableName $! a
  nb <- makeStableName $! b
  pure (na == nb)
{-# NOINLINE sameObject #-}

tyCons :: [C.TyCon]
tyCons = UniqMap.elems testTyConMap

dataCons :: [C.DataCon]
dataCons = concatMap C.tyConDataCons tyCons

-- | A data constructor that is not in 'testTyConMap'
otherDataCon :: C.DataCon
otherDataCon = mkDataCon "Other" 4300 [] [] [intTy] intTy

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.TypeEnv"
  [ testCase "the signatures of type constructors are those of conversion" $
      forM_ tyCons $ \tc -> do
        let nm = C.tyConName tc
        assertScopedTyAlphaEq (C.tyConLegacyKind tc) (tyConKindOf testTypeEnv nm :: Kind 'VoidS)
        show (newTyConSigOf testTypeEnv nm) @?= show (newTyConSig tc)
        show (tyFamEqnsOf testTypeEnv nm) @?= show (tyFamEqns tc)
        assertBool "lookupTyCon" (isJust (lookupTyCon testTypeEnv nm))

  , testCase "the signatures of data constructors are those of conversion" $
      forM_ (otherDataCon : dataCons) $ \dc -> do
        show (dataConSigOf testTypeEnv dc) @?= show (C.dcSig dc)
        assertScopedTyAlphaEq (C.dcLegacyType dc) (dataConType testTypeEnv emptyScope dc)

  , testCase "signatures are converted once" $ do
      assertBool "newTyConSigOf"
        (sameObject (newTyConSigOf testTypeEnv nTyConName) (newTyConSigOf testTypeEnv nTyConName))
      forM_ tyCons $ \tc -> do
        let nm = C.tyConName tc
        assertBool "tyConKindOf"
          (sameObject (tyConKindOf testTypeEnv nm :: Kind 'VoidS) (tyConKindOf testTypeEnv nm))
        assertBool "tyFamEqnsOf"
          (sameObject (tyFamEqnsOf testTypeEnv nm) (tyFamEqnsOf testTypeEnv nm))
      forM_ dataCons $ \dc -> do
        assertBool "dataConSigOf" (sameObject (dataConSigOf testTypeEnv dc) (dataConSigOf testTypeEnv dc))
        assertBool "dataConType"
          (sameObject (dataConType testTypeEnv emptyScope dc) (dataConType testTypeEnv emptyScope dc))
  , testCase "findTyCon errors on unknown type constructors" $
      assertErrorContains "is not in the UniqMap"
        (C.tyConName (findTyCon (mkTypeEnv UniqMap.empty) nTyConName))

    -- The signatures are closed, and moved into the scope they're used in
  , withTests 200 $ testGroup "properties"
      [ testProperty "the types of a TypeEnv are those of conversion, in any scope" $
          forAllBlind Gen.genFreeVars $ \fvs -> withFreeVars fvs $ \env -> do
            let sc = convScope env
                tenv = mkTypeEnv Gen.tyConMap
                agrees expected actual =
                  counterexample (show expected) (C.aeqType expected (toClashType actual))
            conjoin $
              [ agrees (C.tyConLegacyKind tc) (tyConKindOf tenv (C.tyConName tc) `asScopeOf` sc)
              | tc <- UniqMap.elems Gen.tyConMap ] <>
              [ agrees (C.dcLegacyType dc) (dataConType tenv sc dc) | dc <- Gen.dataCons ]
      ]
  ]
 where
  asScopeOf :: Type n -> Scope n -> Type n
  asScopeOf ty _ = ty
