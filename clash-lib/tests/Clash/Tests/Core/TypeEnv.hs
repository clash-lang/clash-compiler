{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the environment of the types of Clash Core: its lookups
  give the signatures that the type and data constructors store, and give
  them as the same objects every time.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.TypeEnv (tests) where

import Control.Monad (forM_)
import Data.Maybe (isJust)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (makeStableName)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Clash.Core.TyCon as C
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Alpha (aeqTy)
import Clash.Core.Syntax
import Clash.Core.TyCon
import Clash.Core.TypeEnv

import Test.Clash.Rewrite (assertErrorContains, nTyConName, showPprU, testTyConMap)
import Test.Clash.Core (TestScope (..), testTypeEnv, withTests)
import qualified Test.Clash.Core.Gen as Gen

-- | Whether two values are the same heap object, after evaluating them
sameObject :: a -> a -> Bool
sameObject a b = unsafePerformIO $ do
  na <- makeStableName $! a
  nb <- makeStableName $! b
  pure (na == nb)
{-# NOINLINE sameObject #-}

-- | Whether two optional values are the same heap object, see 'sameObject'
sameContents :: Maybe a -> Maybe a -> Bool
sameContents (Just a) (Just b) = sameObject a b
sameContents Nothing Nothing = True
sameContents _ _ = False

tyCons :: [C.TyCon]
tyCons = UniqMap.elems testTyConMap

-- | Assert that two types are alpha-equivalent
assertAeqTy :: String -> Type n -> Type n -> Assertion
assertAeqTy what expected actual =
  assertBool (what <> ": expected " <> showPprU expected <> ", got " <> showPprU actual)
    (aeqTy expected actual)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.TypeEnv"
  [ testCase "the signatures of type constructors are those they store" $
      forM_ tyCons $ \tc -> do
        let nm = C.tyConName tc
        assertAeqTy "kind" (C.tyConKind tc) (tyConKindOf testTypeEnv nm)
        show (newTyConSigOf testTypeEnv nm) @?= show (newTyConSig tc)
        show (tyFamEqnsOf testTypeEnv nm) @?= show (tyFamEqns tc)
        assertBool "lookupTyCon" (isJust (lookupTyCon testTypeEnv nm))

  , testCase "signatures are converted once" $ do
      assertBool "newTyConSigOf"
        (sameContents (newTyConSigOf testTypeEnv nTyConName) (newTyConSigOf testTypeEnv nTyConName))
      forM_ tyCons $ \tc -> do
        let nm = C.tyConName tc
        assertBool "tyConKindOf"
          (sameObject (tyConKindOf testTypeEnv nm :: Kind 'VoidS) (tyConKindOf testTypeEnv nm))
        assertBool "tyFamEqnsOf"
          (sameContents (tyFamEqnsOf testTypeEnv nm) (tyFamEqnsOf testTypeEnv nm))
  , testCase "findTyCon errors on unknown type constructors" $
      assertErrorContains "is not in the UniqMap"
        (C.tyConName (findTyCon (mkTypeEnv UniqMap.empty) nTyConName))

    -- The signatures are closed, and moved into the scope they're used in
  , withTests 200 $ testGroup "properties"
      [ testProperty "the types of a TypeEnv are those the constructors store, in any scope" $
          forAllBlind Gen.genTestScope $ \(Gen.SomeScope ts) -> do
            let sc = testScope ts
                tenv = Gen.typeEnv
                agrees expected actual =
                  counterexample (showPprU expected) (aeqTy (sinkClosed expected) actual)
            conjoin $
              [ agrees (C.tyConKind tc) (tyConKindOf tenv (C.tyConName tc) `asScopeOf` sc)
              | tc <- UniqMap.elems Gen.tyConMap ]
      ]
  ]
 where
  asScopeOf :: Type n -> Scope n -> Type n
  asScopeOf ty _ = ty
