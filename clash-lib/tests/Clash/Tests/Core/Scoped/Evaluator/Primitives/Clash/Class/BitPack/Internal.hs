{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator.Primitives.Clash.Class.BitPack.Internal"
  on fixed inputs. The designs of @clash-differential prims@ have none of
  these primitives.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Class.BitPack.Internal
  (tests) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon)
import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (..))
import qualified Clash.Core.Term as C
import Clash.Core.TyCon (TyConName)
import qualified Clash.Core.Type as C
import Clash.Core.TysPrim
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Core.Scoped.TypeEnv (TypeEnv, mkTypeEnv)
import Clash.Unique (Unique)

import Test.Clash.Rewrite
  (assertAlphaEq, localId, mkAlgTyCon, mkDataCon, mkTyConName, numTy, tcApp, testTyConMap)
import Test.Clash.Scoped.Evaluator.Primitives

-- | A boxed number type: its name, the type of its field, and the literal of
-- its field
data Boxed = Boxed
  { bxName :: String
  , bxPrimTy :: C.Type
  , bxLit :: Integer -> Literal
  , bxUniq :: Unique
  }

boxedTyConName :: Boxed -> TyConName
boxedTyConName b = mkTyConName (bxName b) (bxUniq b)

boxedTy :: Boxed -> C.Type
boxedTy b = tcApp (boxedTyConName b) []

boxedDc :: Boxed -> DataCon
boxedDc b =
  mkDataCon (bxName b <> "#") (bxUniq b + 1) [] [] [bxPrimTy b] (boxedTy b)

-- | A boxed number
boxed :: Boxed -> C.Term -> C.Term
boxed b = C.App (C.Data (boxedDc b))

int8B, int16B, int32B, int64B, wordB, word8B, word16B, word32B, word64B, cushortB
  , floatB, doubleB :: Boxed
int8B = Boxed "GHC.Int.Int8" int8PrimTy Int8Literal 6000
int16B = Boxed "GHC.Int.Int16" int16PrimTy Int16Literal 6002
int32B = Boxed "GHC.Int.Int32" int32PrimTy Int32Literal 6004
int64B = Boxed "GHC.Int.Int64" int64PrimTy Int64Literal 6006
wordB = Boxed "GHC.Types.Word" wordPrimTy WordLiteral 6008
word8B = Boxed "GHC.Word.Word8" word8PrimTy Word8Literal 6010
word16B = Boxed "GHC.Word.Word16" word16PrimTy Word16Literal 6012
word32B = Boxed "GHC.Word.Word32" word32PrimTy Word32Literal 6014
word64B = Boxed "GHC.Word.Word64" word64PrimTy Word64Literal 6016
cushortB = Boxed "Foreign.C.Types.CUShort" word16PrimTy Word16Literal 6018
floatB = Boxed "GHC.Types.Float" floatPrimTy (FloatLiteral . fromInteger) 6020
doubleB = Boxed "GHC.Types.Double" doublePrimTy (DoubleLiteral . fromInteger) 6022

-- | The types of the tests, with the boxed number types
bitPackTypeEnv :: TypeEnv
bitPackTypeEnv = mkTypeEnv $ testTyConMap <> UniqMap.fromList
  [ (boxedTyConName b, mkAlgTyCon (boxedTyConName b) [] [boxedDc b])
  | b <- [ int8B, int16B, int32B, int64B, wordB, word8B, word16B, word32B
         , word64B, cushortB, floatB, doubleB ] ]

bitPackPrim :: Text -> C.Type -> C.Type -> C.Term
bitPackPrim nm a r = prim ("Clash.Class.BitPack.Internal." <> nm) (C.mkFunTy a r)

-- | @pack@ of a boxed number whose field is a let-bound literal, which the
-- reduction evaluates with a nested machine
packTest :: Text -> Boxed -> Integer -> Integer -> Integer -> TestTree
packTest nm b n lit expected =
  testCase (Text.unpack nm) $
    let x = localId User "x" 7 (bxPrimTy b)
        arg = boxed b (C.Let (C.NonRec x (C.Literal (bxLit b lit))) (C.Var x))
    in assertAlphaEq (bv n 0 expected)
         (allocatedBody (evalWith bitPackTypeEnv False
           (C.App (bitPackPrim nm (boxedTy b) (bvTy (numTy n))) arg)))

-- | @unpack@ of a bit vector
unpackTest :: Text -> Boxed -> Integer -> Integer -> Integer -> TestTree
unpackTest nm b n val expected =
  testCase (Text.unpack nm) $
    assertAlphaEq (boxed b (C.Literal (bxLit b expected)))
      (evalWith bitPackTypeEnv False
        (C.App (bitPackPrim nm (bvTy (numTy n)) (boxedTy b)) (bv n 0 val)))

doubleBits :: Integer
doubleBits = toInteger (castDoubleToWord64 (-1.5))

floatBits :: Integer
floatBits = toInteger (castFloatToWord32 2.25)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Class.BitPack.Internal"
  [ testGroup "pack"
      -- The reductions don't wrap negative numbers, like those of Clash Core
      [ packTest "packInt8#" int8B 8 (-3) (-3)
      , packTest "packInt16#" int16B 16 300 300
      , packTest "packInt32#" int32B 32 70000 70000
      , packTest "packInt64#" int64B 64 (2 ^ (40 :: Int)) (2 ^ (40 :: Int))
      , packTest "packWord#" wordB 64 12 12
      , packTest "packWord8#" word8B 8 200 200
      , packTest "packWord16#" word16B 16 60000 60000
      , packTest "packWord32#" word32B 32 4000000000 4000000000
      , packTest "packWord64#" word64B 64 (2 ^ (63 :: Int)) (2 ^ (63 :: Int))
      , packTest "packCUShort#" cushortB 16 7 7
      , packTest "packFloat#" floatB 32 floatBits floatBits
      , packTest "packDouble#" doubleB 64 doubleBits doubleBits
      , testCase "a field that isn't a literal is stuck" $
          let x = localId User "x" 7 int8PrimTy
              call = C.App (bitPackPrim "packInt8#" (boxedTy int8B) (bvTy (numTy 8)))
                       (boxed int8B (C.Var x))
          in assertAlphaEq call (evalWith bitPackTypeEnv False call)
      ]
  , testGroup "unpack"
      [ unpackTest "unpackInt8#" int8B 8 0xfd (-3)
      , unpackTest "unpackInt16#" int16B 16 0xffff (-1)
      , unpackTest "unpackInt32#" int32B 32 5 5
      , unpackTest "unpackInt64#" int64B 64 (2 ^ (63 :: Int)) (-(2 ^ (63 :: Int)))
      , unpackTest "unpackWord#" wordB 64 (2 ^ (64 :: Int) - 1) (2 ^ (64 :: Int) - 1)
      , unpackTest "unpackWord8#" word8B 8 0xfd 0xfd
      , unpackTest "unpackWord16#" word16B 16 0xffff 0xffff
      , unpackTest "unpackWord32#" word32B 32 7 7
      , unpackTest "unpackWord64#" word64B 64 (2 ^ (63 :: Int)) (2 ^ (63 :: Int))
      , unpackTest "unpackCUShort#" cushortB 16 9 9
      , unpackTest "unpackFloat#" floatB 32 floatBits floatBits
      , unpackTest "unpackDouble#" doubleB 64 doubleBits doubleBits
      ]
  ]
