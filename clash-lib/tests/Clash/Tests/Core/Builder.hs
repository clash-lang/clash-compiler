{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the builders of scoped Clash Core
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Builder (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import qualified Clash.Core.Basic as C (ConstTy (..), LitTy (..))

import Clash.Core.Builder
import Clash.Core.Sig (DataConSig (..), NewTyConSig (..), TyFamEqn (..))
import Clash.Core.Syntax

import Test.Clash.Rewrite (TestType (..), mkTyConName)

maybeTc :: C.ConstTy
maybeTc = C.TyCon (mkTyConName "Maybe" 2001)

typeB :: TyB
typeB = testTypeB "Type"

-- | @forall k (a :: k). a -> Maybe a@. The keys differ from the raw names
-- the binders get.
nativeTy :: TyB
nativeTy =
  forAllTyB 11 (C.mkUnsafeSystemName "k" 0) typeB $
    forAllTyB 12 (C.mkUnsafeSystemName "a" 0) (varTyB 11) $
      appTyB (appTyB (constTyB C.Arrow) (varTyB 12)) (appTyB (constTyB maybeTc) (varTyB 12))

-- | The raw name of a type variable
varRaw :: Type n -> Maybe Int
varRaw = \case
  VarTy _ _ n -> Just (tyNameRaw n)
  _ -> Nothing

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Builder"
  [ testCase "native type builders number binders in depth-first order" $
      case runClosedType nativeTy of
        ForAllTy bk (ForAllTy ba (AppTy (AppTy (ConstTy C.Arrow) a1) (AppTy _ a2))) -> do
          [tyNameRaw (tyNameOf bk), tyNameRaw (tyNameOf ba)] @?= [1, 2]
          [varRaw (tyBinderKind ba), varRaw a1, varRaw a2] @?= [Just 1, Just 2, Just 2]
        ty -> assertFailure ("unexpected type: " <> show ty)
  , testCase "runDataConSig numbers binders in depth-first order" $
      case runDataConSig [] [(11, C.mkUnsafeSystemName "b" 0, typeB)] [varTyB 11] of
        DataConSig TNil (TCons b TNil) [t] -> do
          tyNameRaw (tyNameOf b) @?= 1
          varRaw t @?= Just 1
        sig -> assertFailure ("unexpected signature: " <> show sig)
  , testCase "runNewTyConSig numbers binders in depth-first order" $
      case runNewTyConSig [(11, C.mkUnsafeSystemName "a" 0, typeB)] (appTyB (constTyB maybeTc) (varTyB 11)) of
        NewTyConSig (TCons a TNil) (AppTy (ConstTy tc) t) -> do
          tyNameRaw (tyNameOf a) @?= 1
          tc @?= maybeTc
          varRaw t @?= Just 1
        sig -> assertFailure ("unexpected signature: " <> show sig)
  , testCase "runTyFamEqn numbers binders in depth-first order" $
      case runTyFamEqn
             [(11, C.mkUnsafeSystemName "k" 0, typeB), (12, C.mkUnsafeSystemName "a" 0, varTyB 11)]
             [varTyB 12] (varTyB 11) of
        TyFamEqn (TCons k (TCons a TNil)) [lhs] rhs -> do
          [tyNameRaw (tyNameOf k), tyNameRaw (tyNameOf a)] @?= [1, 2]
          [varRaw (tyBinderKind a), varRaw lhs, varRaw rhs] @?= [Just 1, Just 2, Just 1]
        eqn -> assertFailure ("unexpected equation: " <> show eqn)
  , testCase "closed types without binders are shared" $
      case appTyB (constTyB maybeTc) (litTyB (C.NumTy 3)) of
        b -> show (runClosedType b) @?= show (AppTy (ConstTy maybeTc) (LitTy (C.NumTy 3)) :: Type 'VoidS)
  ]
