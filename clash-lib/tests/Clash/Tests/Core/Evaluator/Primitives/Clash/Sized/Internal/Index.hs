{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Evaluator.Primitives.Clash.Sized.Internal.Index"
  on fixed inputs, e.g. of the reductions that build their result from the
  type of the primitive, or give an undefined result.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Evaluator.Primitives.Clash.Sized.Internal.Index
  (tests) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Builder (TmB, TyB)
import Clash.Core.Evaluator.Primitives.Util (indexConPrim)

import Test.Clash.Rewrite (indexTcNm)
import Test.Clash.Core.Evaluator.Primitives

indexPrim :: Text -> TyB -> TmB
indexPrim nm = prim ("Clash.Sized.Internal.Index." <> nm)

-- | An index with the given size type: results have the size of the result
-- type of their primitive
indexOfSize :: TyB -> Integer -> Integer -> TmB
indexOfSize sz n val =
  apps (conPrim (indexConPrim indexTcNm)) [Right sz, Left (natLit n), Left (integerLit val)]

-- | @forall n. KnownNat n => t n@ at size @n@
knownNatCall :: Text -> (TyB -> TyB) -> Integer -> [TmB] -> TmB
knownNatCall nm t n as =
  apps (indexPrim nm ty') (Right (numTy n) : Left (natLit n) : map Left as)
 where
  ty' = forAll "n_9001" natKind (\vn -> knownNatTy vn ~> t vn)

-- | @forall m n. Index m -> Index n -> Index (m + n)@ at sizes @m@ and @n@
extendingCall :: Text -> Integer -> Integer -> [TmB] -> TmB
extendingCall nm m n as =
  apps (indexPrim nm ty') ([Right (numTy m), Right (numTy n)] ++ map Left as)
 where
  ty' = forAll "m_9002" natKind $ \vm -> forAll "n_9001" natKind $ \vn ->
    indexTy vm ~> indexTy vn ~> indexTy (addTy vm vn)

-- | @forall n. Index n -> Index n -> Index n@ at size @n@
binCall :: Text -> Integer -> [TmB] -> TmB
binCall nm n as =
  apps (indexPrim nm ty') (Right (numTy n) : map Left as)
 where
  ty' = forAll "n_9001" natKind (\vn -> binTy (indexTy vn) (indexTy vn))

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Evaluator.Primitives.Clash.Sized.Internal.Index"
  [ testCase "unpack# of a bit vector" $
      assertEvalsTo (index 5 3)
        (knownNatCall "unpack#" (\n -> bvTy (numTy 3) ~> indexTy n) 5 [bv 3 0 3])

  , testCase "unpack# out of bounds is undefined" $
      assertEvalsTo (undefinedAt (indexTy (numTy 5)))
        (knownNatCall "unpack#" (\n -> bvTy (numTy 3) ~> indexTy n) 5 [bv 3 0 6])

  , testCase "toEnum# of an Int" $
      assertEvalsTo (index 5 4)
        (knownNatCall "toEnum#" (\n -> boxedIntTy ~> indexTy n) 5 [boxedInt 4])

  , testCase "fromEnum# is an Int" $
      assertEvalsTo (boxedInt 4)
        (knownNatCall "fromEnum#" (\n -> indexTy n ~> boxedIntTy) 5 [index 5 4])

  , testCase "plus# has the size of its result type" $
      assertEvalsTo (indexOfSize (addTy (numTy 4) (numTy 3)) 7 5)
        (extendingCall "plus#" 4 3 [index 4 3, index 3 2])

  , testCase "minus# below zero is undefined" $
      assertEvalsTo (undefinedAt (indexTy (addTy (numTy 4) (numTy 3))))
        (extendingCall "minus#" 4 3 [index 4 1, index 3 2])

  , testCase "times#" $
      assertEvalsTo (indexOfSize (addTy (numTy 4) (numTy 3)) 7 6)
        (extendingCall "times#" 4 3 [index 4 3, index 3 2])

  , testCase "quot#" $
      assertEvalsTo (index 8 3) (binCall "quot#" 8 [index 8 7, index 8 2])

  , testCase "rem#" $
      assertEvalsTo (index 8 1) (binCall "rem#" 8 [index 8 7, index 8 2])

  , testCase "rem# by zero is undefined" $
      assertEvalsTo (undefinedAt (indexTy (numTy 8))) (binCall "rem#" 8 [index 8 7, index 8 0])

  , testCase "toInteger#" $
      assertEvalsTo (integerLit 6)
        (apps (indexPrim "toInteger#" (forAll "n_9001" natKind (\vn -> indexTy vn ~> integerPrimTy)))
           [Right (numTy 8), Left (index 8 6)])
  ]
