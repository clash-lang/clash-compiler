{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Evaluator.Primitives.Clash.Sized.Internal.BitVector"
  on fixed inputs, e.g. of the reductions that build their result from the
  type of the primitive.
-}

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Evaluator.Primitives.Clash.Sized.Internal.BitVector
  (tests) where

import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Builder (TmB, TyB, dataB)
import Clash.Core.Evaluator.Primitives.Util (bvConPrim)

import Test.Clash.Rewrite
  (ghcFalseDataCon, ghcTrueDataCon, mkTyConName, t2TyConName, testTyConMap)
import Test.Clash.Core (assertTermAlphaEq)
import Test.Clash.Core.Evaluator.Primitives

bvPrim :: Text -> TyB -> TmB
bvPrim nm = prim ("Clash.Sized.Internal.BitVector." <> nm)

-- | @forall n. KnownNat n => t n@
knownNatFun :: (TyB -> TyB) -> TyB
knownNatFun t = forAll "n_9001" natKind (\n -> knownNatTy n ~> t n)

-- | A call of a primitive of type @forall n. KnownNat n => ...@ at size @n@
knownNatCall :: Text -> (TyB -> TyB) -> Integer -> [TmB] -> TmB
knownNatCall nm t n as =
  apps (bvPrim nm (knownNatFun t)) (Right (numTy n) : Left (natLit n) : map Left as)

-- | A primitive on two bit vectors of type
-- @forall m n. (KnownNat m, KnownNat n) => BitVector m -> BitVector n -> BitVector r@
-- at sizes @m@ and @n@, with the given result size @r@
extendingCall :: Text -> (TyB -> TyB -> TyB) -> Integer -> Integer -> [TmB] -> TmB
extendingCall nm r m n as =
  apps (bvPrim nm ty')
    ([Right (numTy m), Right (numTy n), Left (natLit m), Left (natLit n)] ++ map Left as)
 where
  ty' = forAll "m_9002" natKind $ \vm -> forAll "n_9001" natKind $ \vn ->
    knownNatTy vm ~> knownNatTy vn ~> bvTy vm ~> bvTy vn ~> bvTy (r vm vn)

-- | A bit vector with the given size type: results have the size of the
-- result type of their primitive
bvOfSize :: TyB -> Integer -> Integer -> Integer -> TmB
bvOfSize sz n msk val =
  apps (conPrim (bvConPrim bvTcNm)) [Right sz, Left (natLit n), Left (natLit msk), Left (integerLit val)]

-- | A comparison of bits
bitCmpTest :: Text -> Integer -> Integer -> Bool -> TestTree
bitCmpTest nm i j r =
  testCase (Text.unpack nm) $
    assertEvalsTo (dataB (if r then ghcTrueDataCon else ghcFalseDataCon))
      (apps (bvPrim nm (binTy bitTy ghcBoolTy)) [Left (bit 0 i), Left (bit 0 j)])

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Evaluator.Primitives.Clash.Sized.Internal.BitVector"
  [ testCase "BV of integers" $
      let ty' = forAll "n_9001" natKind (\n -> integerPrimTy ~> integerPrimTy ~> bvTy n)
      in assertEvalsTo (bv 8 1 5)
           (apps (bvPrim "BV" ty') [Right (numTy 8), Left (integerLit 1), Left (integerLit 5)])

  , testCase "Bit of integers" $
      assertEvalsTo (bit 0 1)
        (apps (bvPrim "Bit" (binTy integerPrimTy bitTy)) [Left (integerLit 0), Left (integerLit 1)])

  , testCase "size# is an Int" $
      assertEvalsTo (boxedInt 4)
        (knownNatCall "size#" (\n -> bvTy n ~> boxedIntTy) 4 [bv 4 0 1])

  , testCase "maxIndex# is an Int" $
      assertEvalsTo (boxedInt 3)
        (knownNatCall "maxIndex#" (\n -> bvTy n ~> boxedIntTy) 4 [bv 4 0 1])

  , bitCmpTest "neq##" 0 1 True
  , bitCmpTest "lt##" 0 1 True
  , bitCmpTest "ge##" 0 1 False
  , bitCmpTest "gt##" 1 0 True
  , bitCmpTest "le##" 1 0 False

  , testCase "toEnum## of an Int" $
      assertEvalsTo (bit 0 1) (apps (bvPrim "toEnum##" (boxedIntTy ~> bitTy)) [Left (boxedInt 3)])

  , testCase "reduceXor#" $
      assertEvalsTo (bit 0 1)
        (knownNatCall "reduceXor#" (\n -> bvTy n ~> bitTy) 4 [bv 4 0 0b0111])

  , testCase "index# of an undefined bit" $
      assertEvalsTo (bit 1 0)
        (knownNatCall "index#" (\n -> bvTy n ~> boxedIntTy ~> bitTy) 4
           [bv 4 0b0100 0b0011, boxedInt 2])

  , testCase "setSlice# sets bits m to n" $
      -- setSlice# :: forall m i n. SNat (m + 1 + i) -> BitVector (m + 1 + i)
      --           -> SNat m -> SNat n -> BitVector (m + 1 - n) -> BitVector (m + 1 + i)
      let subTy a b = tc (mkTyConName "GHC.TypeNats.-" 4007) [a, b]
          ty' =
            forAll "m_9002" natKind $ \vm -> forAll "i_9004" natKind $ \vi ->
              forAll "n_9001" natKind $ \vn ->
                let size = addTy (addTy vm (numTy 1)) vi
                in snatTy size ~> bvTy size ~> snatTy vm ~> snatTy vn
                     ~> bvTy (subTy (addTy vm (numTy 1)) vn) ~> bvTy size
          resSize = addTy (addTy (numTy 3) (numTy 1)) (numTy 0)
      in assertEvalsTo (bvOfSize resSize 4 0b0111 0b1101)
           (apps (bvPrim "setSlice#" ty')
              [ Right (numTy 3), Right (numTy 0), Right (numTy 2)
              , Left (natLit 4), Left (bv 4 0b1111 0b0001)
              , Left (natLit 3), Left (natLit 2), Left (bv 2 0b01 0b11) ])

  , testCase "split# builds the pair of its result type" $
      -- split# :: forall n m. KnownNat n => BitVector (m + n) -> (BitVector m, BitVector n)
      let t2Dc = case fmap tyConDataCons (UniqMap.lookup t2TyConName testTyConMap) of
            Just [dc] -> dc
            _ -> error "T2 has one constructor"
          ty' = forAll "n_9001" natKind $ \vn -> forAll "m_9002" natKind $ \vm ->
            knownNatTy vn ~> bvTy (addTy vm vn) ~> tc t2TyConName [bvTy vm, bvTy vn]
          expected = apps (dataB t2Dc)
            [ Right (bvTy (numTy 3)), Right (bvTy (numTy 2))
            , Left (bv 3 0b001 0b101), Left (bv 2 0b10 0b11) ]
      in assertEvalsTo expected
           (apps (bvPrim "split#" ty')
              [Right (numTy 2), Right (numTy 3), Left (natLit 2), Left (bv 5 0b00110 0b10111)])

  , testCase "plus# has the size of its result type" $
      assertEvalsTo (bvOfSize (addTy (numTy 4) (numTy 1)) 5 0 17)
        (extendingCall "plus#" (\_ _ -> addTy (numTy 4) (numTy 1)) 4 3 [bv 4 0 12, bv 3 0 5])

  , testCase "minus# wraps around" $
      assertEvalsTo (bvOfSize (addTy (numTy 4) (numTy 1)) 5 0 31)
        (extendingCall "minus#" (\_ _ -> addTy (numTy 4) (numTy 1)) 4 3 [bv 4 0 2, bv 3 0 3])

  , testCase "times# has the size of its result type" $
      assertEvalsTo (bvOfSize (addTy (numTy 4) (numTy 3)) 7 0 60)
        (extendingCall "times#" addTy 4 3 [bv 4 0 12, bv 3 0 5])

  , testCase "plus# of undefined bit vectors is stuck" $
      let call = extendingCall "plus#" (\_ _ -> addTy (numTy 4) (numTy 1)) 4 3 [bv 4 1 12, bv 3 0 5]
      in assertTermAlphaEq (closed call) (eval call)

  , testCase "quot# by zero is undefined" $
      assertEvalsTo (undefinedAt (bvTy (numTy 4)))
        (knownNatCall "quot#" (\n -> binTy (bvTy n) (bvTy n)) 4 [bv 4 0 12, bv 4 0 0])
  ]
