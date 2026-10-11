{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.BitVector"
  on fixed inputs: the reductions that build their result from the type of
  the primitive, and those whose primitives the designs of
  @clash-differential prims@ don't have.
-}

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.BitVector
  (tests) where

import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (mkUnsafeSystemName)
import qualified Clash.Core.Term as C
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Core.Type as C
import Clash.Core.TysPrim (integerPrimTy, typeNatKind)
import qualified Clash.Core.Var as C
import qualified Clash.Data.UniqMap as UniqMap
import qualified Clash.Normalize.Primitives as NP

import Clash.Core.Scoped.Evaluator.Primitives.Util (bvConPrim)

import Test.Clash.Rewrite
  ( addTy, assertAlphaEq, ghcBoolTy, ghcFalseDataCon, ghcTrueDataCon, mkTyConName, numTy
  , snatTy, t2TyConName, tcApp, testTyConMap )
import Test.Clash.Scoped.Evaluator.Primitives

bvPrim :: Text -> C.Type -> C.Term
bvPrim nm = prim ("Clash.Sized.Internal.BitVector." <> nm)

-- | Evaluate with 'sizedTypeEnv'
evalSized :: C.Term -> C.Term
evalSized = evalWith sizedTypeEnv False

-- | @forall n. KnownNat n => t n@
knownNatFun :: (C.Type -> C.Type) -> C.Type
knownNatFun t = C.ForAllTy tvN (C.mkFunTy (knownNatTy (C.VarTy tvN)) (t (C.VarTy tvN)))

-- | A call of a primitive of type @forall n. KnownNat n => ...@ at size @n@
knownNatCall :: Text -> (C.Type -> C.Type) -> Integer -> [C.Term] -> C.Term
knownNatCall nm t n as =
  C.mkApps (bvPrim nm (knownNatFun t)) (Right (numTy n) : Left (natLit n) : map Left as)

-- | A primitive on two bit vectors of type
-- @forall m n. (KnownNat m, KnownNat n) => BitVector m -> BitVector n -> BitVector r@
-- at sizes @m@ and @n@, with the given result size @r@
extendingCall :: Text -> (C.Type -> C.Type -> C.Type) -> Integer -> Integer -> [C.Term] -> C.Term
extendingCall nm r m n as =
  C.mkApps (bvPrim nm ty)
    ([Right (numTy m), Right (numTy n), Left (natLit m), Left (natLit n)] ++ map Left as)
 where
  vm = C.VarTy tvM
  vn = C.VarTy tvN
  ty = C.ForAllTy tvM $ C.ForAllTy tvN $
    C.mkFunTy (knownNatTy vm) $ C.mkFunTy (knownNatTy vn) $
      C.mkFunTy (bvTy vm) $ C.mkFunTy (bvTy vn) (bvTy (r vm vn))

-- | A bit vector with the given size type: results have the size of the
-- result type of their primitive
bvOfSize :: C.Type -> Integer -> Integer -> Integer -> C.Term
bvOfSize sz n msk val =
  C.mkApps (closedPrimTerm (bvConPrim bvTcNm))
    [Right sz, Left (natLit n), Left (natLit msk), Left (integerLit val)]

tvI :: C.TyVar
tvI = C.mkTyVar typeNatKind (mkUnsafeSystemName "i" 9004)

-- | A comparison of bits
bitCmpTest :: Text -> Integer -> Integer -> Bool -> TestTree
bitCmpTest nm i j r =
  testCase (Text.unpack nm) $
    assertAlphaEq (C.Data (if r then ghcTrueDataCon else ghcFalseDataCon))
      (eval (C.mkApps (bvPrim nm (binTy bitTy ghcBoolTy)) [Left (bit 0 i), Left (bit 0 j)]))

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.BitVector"
  [ testCase "BV of integers" $
      let ty = C.ForAllTy tvN $
            C.mkFunTy integerPrimTy (C.mkFunTy integerPrimTy (bvTy (C.VarTy tvN)))
      in assertAlphaEq (bv 8 1 5)
           (eval (C.mkApps (bvPrim "BV" ty)
                    [Right (numTy 8), Left (integerLit 1), Left (integerLit 5)]))

  , testCase "Bit of integers" $
      assertAlphaEq (bit 0 1)
        (eval (C.mkApps (bvPrim "Bit" (binTy integerPrimTy bitTy))
                 [Left (integerLit 0), Left (integerLit 1)]))

  , testCase "size# is an Int" $
      assertAlphaEq (boxedInt 4)
        (evalSized (knownNatCall "size#" (\n -> C.mkFunTy (bvTy n) boxedIntTy) 4 [bv 4 0 1]))

  , testCase "maxIndex# is an Int" $
      assertAlphaEq (boxedInt 3)
        (evalSized (knownNatCall "maxIndex#" (\n -> C.mkFunTy (bvTy n) boxedIntTy) 4 [bv 4 0 1]))

  , bitCmpTest "neq##" 0 1 True
  , bitCmpTest "lt##" 0 1 True
  , bitCmpTest "ge##" 0 1 False
  , bitCmpTest "gt##" 1 0 True
  , bitCmpTest "le##" 1 0 False

  , testCase "toEnum## of an Int" $
      assertAlphaEq (bit 0 1)
        (evalSized (C.App (bvPrim "toEnum##" (C.mkFunTy boxedIntTy bitTy)) (boxedInt 3)))

  , testCase "reduceXor#" $
      assertAlphaEq (bit 0 1)
        (eval (knownNatCall "reduceXor#" (\n -> C.mkFunTy (bvTy n) bitTy) 4 [bv 4 0 0b0111]))

  , testCase "index# of an undefined bit" $
      assertAlphaEq (bit 1 0)
        (evalSized (knownNatCall "index#" (\n -> C.mkFunTy (bvTy n) (C.mkFunTy boxedIntTy bitTy)) 4
                      [bv 4 0b0100 0b0011, boxedInt 2]))

  , testCase "setSlice# sets bits m to n" $
      -- setSlice# :: forall m i n. SNat (m + 1 + i) -> BitVector (m + 1 + i)
      --           -> SNat m -> SNat n -> BitVector (m + 1 - n) -> BitVector (m + 1 + i)
      let subTy a b = tcApp (mkTyConName "GHC.TypeNats.-" 4007) [a, b]
          vm = C.VarTy tvM
          vi = C.VarTy tvI
          vn = C.VarTy tvN
          size = addTy (addTy vm (numTy 1)) vi
          ty = C.ForAllTy tvM $ C.ForAllTy tvI $ C.ForAllTy tvN $
            C.mkFunTy (snatTy size) $ C.mkFunTy (bvTy size) $
              C.mkFunTy (snatTy vm) $ C.mkFunTy (snatTy vn) $
                C.mkFunTy (bvTy (subTy (addTy vm (numTy 1)) vn)) (bvTy size)
          resSize = addTy (addTy (numTy 3) (numTy 1)) (numTy 0)
      in assertAlphaEq (bvOfSize resSize 4 0b0111 0b1101)
           (eval (C.mkApps (bvPrim "setSlice#" ty)
                    [ Right (numTy 3), Right (numTy 0), Right (numTy 2)
                    , Left (natLit 4), Left (bv 4 0b1111 0b0001)
                    , Left (natLit 3), Left (natLit 2), Left (bv 2 0b01 0b11) ]))

  , testCase "split# builds the pair of its result type" $
      -- split# :: forall n m. KnownNat n => BitVector (m + n) -> (BitVector m, BitVector n)
      let t2Dc = case fmap tyConDataCons (UniqMap.lookup t2TyConName testTyConMap) of
            Just [dc] -> dc
            _ -> error "T2 has one constructor"
          vn = C.VarTy tvN
          vm = C.VarTy tvM
          ty = C.ForAllTy tvN $ C.ForAllTy tvM $
            C.mkFunTy (knownNatTy vn) $
              C.mkFunTy (bvTy (addTy vm vn)) (tcApp t2TyConName [bvTy vm, bvTy vn])
          expected = C.mkApps (C.Data t2Dc)
            [ Right (bvTy (numTy 3)), Right (bvTy (numTy 2))
            , Left (bv 3 0b001 0b101), Left (bv 2 0b10 0b11) ]
      in assertAlphaEq expected
           (eval (C.mkApps (bvPrim "split#" ty)
                    [ Right (numTy 2), Right (numTy 3), Left (natLit 2)
                    , Left (bv 5 0b00110 0b10111) ]))

  , testCase "plus# has the size of its result type" $
      assertAlphaEq (bvOfSize (addTy (numTy 4) (numTy 1)) 5 0 17)
        (eval (extendingCall "plus#" (\_ _ -> addTy (numTy 4) (numTy 1)) 4 3 [bv 4 0 12, bv 3 0 5]))

  , testCase "minus# wraps around" $
      assertAlphaEq (bvOfSize (addTy (numTy 4) (numTy 1)) 5 0 31)
        (eval (extendingCall "minus#" (\_ _ -> addTy (numTy 4) (numTy 1)) 4 3 [bv 4 0 2, bv 3 0 3]))

  , testCase "times# has the size of its result type" $
      assertAlphaEq (bvOfSize (addTy (numTy 4) (numTy 3)) 7 0 60)
        (eval (extendingCall "times#" addTy 4 3 [bv 4 0 12, bv 3 0 5]))

  , testCase "plus# of undefined bit vectors is stuck" $
      let call = extendingCall "plus#" (\_ _ -> addTy (numTy 4) (numTy 1)) 4 3 [bv 4 1 12, bv 3 0 5]
      in assertAlphaEq call (eval call)

  , testCase "quot# by zero is undefined" $
      assertAlphaEq (C.TyApp (C.Prim NP.undefined) (bvTy (numTy 4)))
        (eval (knownNatCall "quot#" (\n -> binTy (bvTy n) (bvTy n)) 4 [bv 4 0 12, bv 4 0 0]))
  ]
