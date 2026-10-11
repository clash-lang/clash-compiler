{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.Index"
  on fixed inputs: the reductions that build their result from the type of
  the primitive, give an undefined result, or whose primitives the designs of
  @clash-differential prims@ don't have.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.Index
  (tests) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Core.TysPrim (integerPrimTy)
import qualified Clash.Normalize.Primitives as NP

import Clash.Core.Scoped.Evaluator.Primitives.Util (indexConPrim)

import Test.Clash.Rewrite (addTy, assertAlphaEq, numTy)
import Test.Clash.Scoped.Evaluator.Primitives

indexPrim :: Text -> C.Type -> C.Term
indexPrim nm = prim ("Clash.Sized.Internal.Index." <> nm)

-- | Evaluate with 'sizedTypeEnv'
evalSized :: C.Term -> C.Term
evalSized = evalWith sizedTypeEnv False

-- | An index with the given size type: results have the size of the result
-- type of their primitive
indexOfSize :: C.Type -> Integer -> Integer -> C.Term
indexOfSize sz n val =
  C.mkApps (closedPrimTerm (indexConPrim indexTcNm))
    [Right sz, Left (natLit n), Left (integerLit val)]

-- | @forall n. KnownNat n => t n@ at size @n@
knownNatCall :: Text -> (C.Type -> C.Type) -> Integer -> [C.Term] -> C.Term
knownNatCall nm t n as =
  C.mkApps (indexPrim nm ty) (Right (numTy n) : Left (natLit n) : map Left as)
 where
  ty = C.ForAllTy tvN (C.mkFunTy (knownNatTy (C.VarTy tvN)) (t (C.VarTy tvN)))

-- | @forall m n. Index m -> Index n -> Index (m + n)@ at sizes @m@ and @n@
extendingCall :: Text -> Integer -> Integer -> [C.Term] -> C.Term
extendingCall nm m n as =
  C.mkApps (indexPrim nm ty) ([Right (numTy m), Right (numTy n)] ++ map Left as)
 where
  vm = C.VarTy tvM
  vn = C.VarTy tvN
  ty = C.ForAllTy tvM $ C.ForAllTy tvN $
    C.mkFunTy (indexTy vm) $ C.mkFunTy (indexTy vn) (indexTy (addTy vm vn))

-- | @forall n. Index n -> Index n -> Index n@ at size @n@
binCall :: Text -> Integer -> [C.Term] -> C.Term
binCall nm n as =
  C.mkApps (indexPrim nm ty) (Right (numTy n) : map Left as)
 where
  ty = C.ForAllTy tvN (binTy (indexTy (C.VarTy tvN)) (indexTy (C.VarTy tvN)))

undefinedIndex :: C.Type -> C.Term
undefinedIndex sz = C.TyApp (C.Prim NP.undefined) (indexTy sz)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.Index"
  [ testCase "unpack# of a bit vector" $
      assertAlphaEq (index 5 3)
        (eval (knownNatCall "unpack#" (\n -> C.mkFunTy (bvTy (numTy 3)) (indexTy n)) 5 [bv 3 0 3]))

  , testCase "unpack# out of bounds is undefined" $
      assertAlphaEq (undefinedIndex (numTy 5))
        (eval (knownNatCall "unpack#" (\n -> C.mkFunTy (bvTy (numTy 3)) (indexTy n)) 5 [bv 3 0 6]))

  , testCase "toEnum# of an Int" $
      assertAlphaEq (index 5 4)
        (evalSized (knownNatCall "toEnum#" (C.mkFunTy boxedIntTy . indexTy) 5 [boxedInt 4]))

  , testCase "fromEnum# is an Int" $
      assertAlphaEq (boxedInt 4)
        (evalSized (knownNatCall "fromEnum#" (\n -> C.mkFunTy (indexTy n) boxedIntTy) 5 [index 5 4]))

  , testCase "plus# has the size of its result type" $
      assertAlphaEq (indexOfSize (addTy (numTy 4) (numTy 3)) 7 5)
        (eval (extendingCall "plus#" 4 3 [index 4 3, index 3 2]))

  , testCase "minus# below zero is undefined" $
      assertAlphaEq (undefinedIndex (addTy (numTy 4) (numTy 3)))
        (eval (extendingCall "minus#" 4 3 [index 4 1, index 3 2]))

  , testCase "times#" $
      assertAlphaEq (indexOfSize (addTy (numTy 4) (numTy 3)) 7 6)
        (eval (extendingCall "times#" 4 3 [index 4 3, index 3 2]))

  , testCase "quot#" $
      assertAlphaEq (index 8 3)
        (eval (binCall "quot#" 8 [index 8 7, index 8 2]))

  , testCase "rem#" $
      assertAlphaEq (index 8 1)
        (eval (binCall "rem#" 8 [index 8 7, index 8 2]))

  , testCase "rem# by zero is undefined" $
      assertAlphaEq (undefinedIndex (numTy 8))
        (eval (binCall "rem#" 8 [index 8 7, index 8 0]))

  , testCase "toInteger#" $
      assertAlphaEq (integerLit 6)
        (eval (C.mkApps (indexPrim "toInteger#"
                           (C.ForAllTy tvN (C.mkFunTy (indexTy (C.VarTy tvN)) integerPrimTy)))
                 [Right (numTy 8), Left (index 8 6)]))
  ]
