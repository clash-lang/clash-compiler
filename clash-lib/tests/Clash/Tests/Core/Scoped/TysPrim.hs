{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the builtin types of scoped Clash Core, and their type
  constructors
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.TysPrim (tests) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import Clash.Core.DataCon (DataCon (..), DcStrictness (..))
import Clash.Core.TyCon (TyCon (..), TyConName, tyConDataCons)
import qualified Clash.Core.Basic as C (ConstTy (TyCon))
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Unique (Unique, fromGhcUnique)
import qualified GHC.Builtin.Names as GHC

import Clash.Core.Scoped.Alpha (aeqTy)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Type (mkFunTy)
import Clash.Core.Scoped.TysPrim

import Test.Clash.Scoped (assertTypeIs, emptyTestScope)
import Test.Clash.Rewrite (ghcName)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.TysPrim"
  [ testCase "builtin types are the type constructors of GHC" $
      mapM_ builtinIs builtins
  , testCase "undefinedTy" $ assertTypeIs emptyTestScope "forall (a :: Type). a" undefinedTy
  , testCase "unsafeCoerceTy" $
      assertTypeIs emptyTestScope "forall (a :: Type) (b :: Type). a -> b" unsafeCoerceTy
  , testCase "kinds of the type constructors" $
      mapM_ kindIs (UniqMap.elems tysPrimMap)
  , testCase "data constructors of Integer" $
      dataConsAre integerPrimTyConName integerIsDc integerPrimTy
        [intPrimTy, byteArrayPrimTy, byteArrayPrimTy]
  , testCase "data constructors of Natural" $
      dataConsAre naturalPrimTyConName naturalNsDc naturalPrimTy
        [wordPrimTy, byteArrayPrimTy]
  ]

-- | The data constructors of a type constructor: the first one is the given
-- one, they are tagged in order, and each has one strict field of the given
-- type
dataConsAre :: TyConName -> DataCon -> Type VoidS -> [Type VoidS] -> Assertion
dataConsAre tc dc0 resTy argTys = do
  let dcs = maybe [] tyConDataCons (UniqMap.lookup tc tysPrimMap)
  map dcName (take 1 dcs) @?= [dcName dc0]
  map dcTag dcs @?= [1 .. length argTys]
  mapM_ (\(dc, argTy) -> do
           assertBool ("type of " <> show (dcName dc))
             (aeqTy (mkFunTy argTy resTy) (dcType dc))
           assertBool "strict field" (dcArgStrict dc == [Strict]))
    (zip dcs argTys)

-- | The kind of a builtin type constructor: that of the equality constructor
-- takes two types of any kind, the others are of kind @Type@
kindIs :: TyCon -> Assertion
kindIs tc
  | tyConName tc == eqPrimTyConName =
      assertTypeIs emptyTestScope "forall (a :: Type) (b :: Type). a -> b -> Type" (tyConKind tc)
  | otherwise = assertTypeIs emptyTestScope "Type" (tyConKind tc)

-- | Assert that a builtin type is the type constructor with the given name
-- (see 'ghcName') and unique: that of GHCs type constructor
builtinIs :: (Type VoidS, Text, Unique) -> Assertion
builtinIs (ty, occ, u) = case ty of
  ConstTy (C.TyCon nm) -> (C.nameOcc nm, C.nameUniq nm, C.nameSort nm) @?= (ghcName occ, u, C.System)
  _ -> assertFailure ("not a type constructor: " <> show ty)

builtins :: [(Type VoidS, Text, Unique)]
builtins =
  [ (liftedTypeKind, "Type", fromGhcUnique GHC.liftedTypeKindTyConKey)
  , (typeNatKind, "GHC.Num.Natural.Natural", fromGhcUnique GHC.naturalTyConKey)
  , (typeSymbolKind, "Symbol", fromGhcUnique GHC.typeSymbolKindConNameKey)
  , (intPrimTy, "GHC.Prim.Int#", fromGhcUnique GHC.intPrimTyConKey)
  , (integerPrimTy, "GHC.Num.Integer.Integer", fromGhcUnique GHC.integerTyConKey)
  , (charPrimTy, "GHC.Prim.Char#", fromGhcUnique GHC.charPrimTyConKey)
  , (stringPrimTy, "GHC.Prim.Addr#", fromGhcUnique GHC.addrPrimTyConKey)
  , (wordPrimTy, "GHC.Prim.Word#", fromGhcUnique GHC.wordPrimTyConKey)
  , (int64PrimTy, "GHC.Prim.Int64#", fromGhcUnique GHC.int64PrimTyConKey)
  , (word64PrimTy, "GHC.Prim.Word64#", fromGhcUnique GHC.word64PrimTyConKey)
  , (int8PrimTy, "GHC.Prim.Int8#", fromGhcUnique GHC.int8PrimTyConKey)
  , (int16PrimTy, "GHC.Prim.Int16#", fromGhcUnique GHC.int16PrimTyConKey)
  , (int32PrimTy, "GHC.Prim.Int32#", fromGhcUnique GHC.int32PrimTyConKey)
  , (word8PrimTy, "GHC.Prim.Word8#", fromGhcUnique GHC.word8PrimTyConKey)
  , (word16PrimTy, "GHC.Prim.Word16#", fromGhcUnique GHC.word16PrimTyConKey)
  , (word32PrimTy, "GHC.Prim.Word32#", fromGhcUnique GHC.word32PrimTyConKey)
  , (floatPrimTy, "GHC.Prim.Float#", fromGhcUnique GHC.floatPrimTyConKey)
  , (doublePrimTy, "GHC.Prim.Double#", fromGhcUnique GHC.doublePrimTyConKey)
  , (naturalPrimTy, "GHC.Num.Natural.Natural", fromGhcUnique GHC.naturalTyConKey)
  , (byteArrayPrimTy, "GHC.Prim.ByteArray#", fromGhcUnique GHC.byteArrayPrimTyConKey)
  , (eqPrimTy, "GHC.Prim.~#", fromGhcUnique GHC.eqPrimTyConKey)
  ]
