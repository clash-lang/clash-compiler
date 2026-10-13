{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for type constructors in scoped Clash Core
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.TyCon (tests) where

import Data.Maybe (fromMaybe)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.TyCon as C
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Syntax
import Clash.Core.TyCon

import Test.Clash.Rewrite (intTyCon, nTyConName, pTyConName, testTyConMap)
import Test.Clash.Scoped
  (TestScope (..), assertTypeIs, atRaw, closedType, emptyTestScope, typeIn, tyVarD, withTestScope)

-- | A type constructor of 'testTyConMap'
testTyCon :: C.TyConName -> C.TyCon
testTyCon nm = fromMaybe (error "testTyCon: not in testTyConMap") (UniqMap.lookup nm testTyConMap)

-- | @newtype N a = MkN (Maybe a)@
nTyCon :: C.TyCon
nTyCon = testTyCon nTyConName

-- | @newtype P a = MkP (forall b. a -> b)@
pTyCon :: C.TyCon
pTyCon = testTyCon pTyConName

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.TyCon"
  [ testCase "newTyConSig" $
      case newTyConSig nTyCon of
        Just (NewTyConSig (TCons a TNil) rhs) -> do
          case rhs of
            AppTy _ (VarTy _ _ n) -> tyNameRaw n @?= tyNameRaw (tyNameOf a)
            _ -> assertFailure "unexpected representation type"
        _ -> assertFailure "unexpected signature"

  , testCase "newTyConSig of a type that is not a newtype" $
      assertBool "not a newtype" (null (newTyConSig intTyCon))

  , testCase "newTyConInstRhs" $ do
      let int = closedType "Int"
      sig <- maybe (assertFailure "no signature") pure (newTyConSig nTyCon)
      case newTyConInstRhs emptyScope sig [int] of
        Just ty -> assertTypeIs emptyTestScope "Maybe Int" ty
        Nothing -> assertFailure "no instantiation"
      -- More types than type variables are applied to the result
      case newTyConInstRhs emptyScope sig [int, int] of
        Just ty -> assertTypeIs emptyTestScope "Maybe Int Int" ty
        Nothing -> assertFailure "no instantiation"
      assertBool "too few types" (null (newTyConInstRhs emptyScope sig []))

    -- Instantiating @a := c@, where the free type variable @c@ has the same
    -- name as the @b@ the representation type binds, must rename @b@.
  , testCase "newTyConInstRhs renames binders that would capture" $ do
      sig <- maybe (assertFailure "no signature") pure (newTyConSig pTyCon)
      -- In the empty scope, a gets name 1 and b gets name 2
      withTestScope [atRaw 2 (tyVarD "c")] $ \ts ->
        case newTyConInstRhs (testScope ts) sig [typeIn ts "c"] of
          Just ty -> assertTypeIs ts "forall (d :: Type). c -> d" ty
          Nothing -> assertFailure "no instantiation"

  , testCase "tyFamEqns of a type that is not a type family" $
      assertBool "not a type family" (null (tyFamEqns nTyCon))
  ]
