{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the builtin types of scoped Clash Core, and their type
  constructors: they agree with those of "Clash.Core.TysPrim", as it made them
  before the scoped ones replaced them.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.TysPrim (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import Clash.Core.DataCon (DataCon (..), DcStrictness (..))
import Clash.Core.TyCon (TyCon (..), TyConName, tyConDataCons)
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Util as C
import qualified Clash.Core.Var as C
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TysPrim

import Test.Clash.Scoped (assertScopedTyAlphaEq)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.TysPrim"
  [ testCase "builtin types agree with Clash.Core.TysPrim" $
      mapM_ (uncurry assertScopedTyAlphaEq) builtins
  , testCase "undefinedTy" $ assertScopedTyAlphaEq C.undefinedTy undefinedTy
  , testCase "unsafeCoerceTy" $ assertScopedTyAlphaEq C.unsafeCoerceTy unsafeCoerceTy
  , testCase "kinds of the type constructors" $
      mapM_ (\tc -> assertScopedTyAlphaEq (expectedKind tc) (tyConKind tc))
        (UniqMap.elems tysPrimMap)
  , testCase "data constructors of Integer" $
      dataConsAgree integerPrimTyConName integerIsDc C.integerPrimTy
        [C.intPrimTy, C.byteArrayPrimTy, C.byteArrayPrimTy]
  , testCase "data constructors of Natural" $
      dataConsAgree naturalPrimTyConName naturalNsDc C.naturalPrimTy
        [C.wordPrimTy, C.byteArrayPrimTy]
  ]

-- | The data constructors of a type constructor: the first one is the given
-- one, they are tagged in order, and each has one strict field of the given
-- type
dataConsAgree :: TyConName -> DataCon -> C.Type -> [C.Type] -> Assertion
dataConsAgree tc dc0 resTy argTys = do
  let dcs = maybe [] tyConDataCons (UniqMap.lookup tc tysPrimMap)
  map dcName (take 1 dcs) @?= [dcName dc0]
  map dcTag dcs @?= [1 .. length argTys]
  mapM_ (\(dc, argTy) -> do
           assertScopedTyAlphaEq (C.mkPolyFunTy resTy [Right argTy]) (dcType dc)
           assertBool "strict field" (dcArgStrict dc == [Strict]))
    (zip dcs argTys)

-- | The kinds that "Clash.Core.TysPrim" gave the type constructors
expectedKind :: TyCon -> C.Kind
expectedKind tc
  | tyConName tc == eqPrimTyConName =
      C.mkPolyFunTy C.liftedTypeKind
        [Left aTv, Left bTv, Right (C.VarTy aTv), Right (C.VarTy bTv)]
  | otherwise = C.liftedTypeKind
 where
  aTv = C.mkTyVar C.liftedTypeKind (C.mkUnsafeSystemName "a" 0)
  bTv = C.mkTyVar C.liftedTypeKind (C.mkUnsafeSystemName "b" 1)

builtins :: [(C.Type, Type 'VoidS)]
builtins =
  [ (C.liftedTypeKind, liftedTypeKind)
  , (C.typeNatKind, typeNatKind)
  , (C.typeSymbolKind, typeSymbolKind)
  , (C.intPrimTy, intPrimTy)
  , (C.integerPrimTy, integerPrimTy)
  , (C.charPrimTy, charPrimTy)
  , (C.stringPrimTy, stringPrimTy)
  , (C.wordPrimTy, wordPrimTy)
  , (C.int64PrimTy, int64PrimTy)
  , (C.word64PrimTy, word64PrimTy)
  , (C.int8PrimTy, int8PrimTy)
  , (C.int16PrimTy, int16PrimTy)
  , (C.int32PrimTy, int32PrimTy)
  , (C.word8PrimTy, word8PrimTy)
  , (C.word16PrimTy, word16PrimTy)
  , (C.word32PrimTy, word32PrimTy)
  , (C.floatPrimTy, floatPrimTy)
  , (C.doublePrimTy, doublePrimTy)
  , (C.naturalPrimTy, naturalPrimTy)
  , (C.byteArrayPrimTy, byteArrayPrimTy)
  , (C.eqPrimTy, eqPrimTy)
  ]
