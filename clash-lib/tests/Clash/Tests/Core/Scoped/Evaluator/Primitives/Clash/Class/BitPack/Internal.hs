{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Evaluator.Primitives.Clash.Class.BitPack.Internal"
  on fixed inputs.
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
import Clash.Core.TyCon (AlgTyConRhs (DataTyCon), TyConName)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Unique (Unique)

import Clash.Core.Builder (TmB, TyB, appB, dataB, literalB)
import Clash.Core.TyCon (mkAlgTyCon)
import Clash.Core.Evaluator.Types (Env)
import Clash.Core.TypeEnv (mkTypeEnv)
import Clash.Core.TysPrim (liftedTypeKind)
import Clash.Rewrite.Scoped.Whnf (evaluatorEnv)

import Test.Clash.Rewrite (mkTyConName, testTyConMap)
import Test.Clash.Scoped (assertTermAlphaEq, assertTermIs, builtTerm)
import Test.Clash.Scoped.Evaluator (allocatedBody)
import Test.Clash.Scoped.Evaluator.Primitives

-- | A boxed number type: its name, the type of its field, and the literal of
-- its field
data Boxed = Boxed
  { bxName :: String
  , bxPrimTy :: TyB
  , bxLit :: Integer -> Literal
  , bxUniq :: Unique
  }

boxedTyConName :: Boxed -> TyConName
boxedTyConName b = mkTyConName (bxName b) (bxUniq b)

boxedTy :: Boxed -> TyB
boxedTy b = tc (boxedTyConName b) []

boxedDc :: Boxed -> DataCon
boxedDc b = monoDataCon (bxName b <> "#") (bxUniq b + 1) [bxPrimTy b] (boxedTy b)

-- | A boxed number
boxed :: Boxed -> TmB -> TmB
boxed b = appB (dataB (boxedDc b))

int8B, int16B, int32B, int64B, wordB, word8B, word16B, word32B, word64B, cushortB
  , floatB, doubleB :: Boxed
int8B = Boxed "GHC.Int.Int8" (ty "GHC.Prim.Int8#") Int8Literal 6000
int16B = Boxed "GHC.Int.Int16" (ty "GHC.Prim.Int16#") Int16Literal 6002
int32B = Boxed "GHC.Int.Int32" (ty "GHC.Prim.Int32#") Int32Literal 6004
int64B = Boxed "GHC.Int.Int64" (ty "GHC.Prim.Int64#") Int64Literal 6006
wordB = Boxed "GHC.Types.Word" wordPrimTy WordLiteral 6008
word8B = Boxed "GHC.Word.Word8" (ty "GHC.Prim.Word8#") Word8Literal 6010
word16B = Boxed "GHC.Word.Word16" (ty "GHC.Prim.Word16#") Word16Literal 6012
word32B = Boxed "GHC.Word.Word32" (ty "GHC.Prim.Word32#") Word32Literal 6014
word64B = Boxed "GHC.Word.Word64" (ty "GHC.Prim.Word64#") Word64Literal 6016
cushortB = Boxed "Foreign.C.Types.CUShort" (ty "GHC.Prim.Word16#") Word16Literal 6018
floatB = Boxed "GHC.Types.Float" (ty "GHC.Prim.Float#") (FloatLiteral . fromInteger) 6020
doubleB = Boxed "GHC.Types.Double" doublePrimTy (DoubleLiteral . fromInteger) 6022

-- | The environment of the evaluator with the types of the tests and the
-- boxed number types
bitPackEnv :: Env
bitPackEnv = evaluatorEnv $ mkTypeEnv $ testTyConMap <> UniqMap.fromList
  [ (boxedTyConName b, mkAlgTyCon (boxedTyConName b) liftedTypeKind 0 (DataTyCon [boxedDc b]) False)
  | b <- [ int8B, int16B, int32B, int64B, wordB, word8B, word16B, word32B
         , word64B, cushortB, floatB, doubleB ] ]

bitPackPrim :: Text -> TyB -> TyB -> TmB
bitPackPrim nm a r = prim ("Clash.Class.BitPack.Internal." <> nm) (a ~> r)

-- | @pack@ of a boxed number whose field is a let-bound literal, which the
-- reduction evaluates with a nested machine
packTest :: Text -> Boxed -> Integer -> Integer -> Integer -> TestTree
packTest nm b n lit expected =
  testCase (Text.unpack nm) $
    let arg = boxed b (letNonRec "x_7" (bxPrimTy b) (literalB (bxLit b lit)) id)
    in assertTermAlphaEq (closed (bv n 0 expected))
         (allocatedBody (evalEnvIn bitPackEnv False
           (appB (bitPackPrim nm (boxedTy b) (bvTy (numTy n))) arg)))

-- | @unpack@ of a bit vector
unpackTest :: Text -> Boxed -> Integer -> Integer -> Integer -> TestTree
unpackTest nm b n val expected =
  testCase (Text.unpack nm) $
    assertTermAlphaEq (closed (boxed b (literalB (bxLit b expected))))
      (evalEnvIn bitPackEnv False
        (appB (bitPackPrim nm (bvTy (numTy n)) (boxedTy b)) (bv n 0 val)))

doubleBits :: Integer
doubleBits = toInteger (castDoubleToWord64 (-1.5))

floatBits :: Integer
floatBits = toInteger (castFloatToWord32 2.25)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Class.BitPack.Internal"
  [ testGroup "pack"
      -- The reductions don't wrap negative numbers
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
          let call = appB (bitPackPrim "packInt8#" (boxedTy int8B) (bvTy (numTy 8)))
                       (boxed int8B (var "x_7"))
          in evalOpenIn bitPackEnv [("x_7", bxPrimTy int8B)] False call $ \ts r ->
               assertTermIs ts (builtTerm call) r
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
