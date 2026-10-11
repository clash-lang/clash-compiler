{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector", on
  fixed inputs. @clash-differential prims --open@ compares the reductions with
  those of Clash Core on generated calls.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector (tests) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (..), mkUnsafeSystemName)
import Clash.Core.Pretty (showPpr)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Core.TysPrim (integerPrimTy, liftedTypeKind, naturalPrimTy, typeNatKind)
import qualified Clash.Core.Util as C (mkVec)
import qualified Clash.Core.Var as C

import Test.Clash.Rewrite
  ( consDc, iHashDc, hIntTy, heapLookupEvaluator, knownNatTy, kindedTyVar, nilDc, numTy
  , snatDc, snatTy, vecTy )
import Test.Clash.Scoped (testTypeEnv)
import Test.Clash.Scoped.Evaluator (portedEnv, sameResult, scopedWhnf)

-- | Evaluate with the ported reductions, as the subject of a case-expression
evalSubj :: C.Term -> C.Term
evalSubj = scopedWhnf (portedEnv testTypeEnv heapLookupEvaluator) mempty True

-- | A primitive with the given name and type
prim :: Text -> C.Type -> C.Term
prim nm ty = C.Prim (C.PrimInfo nm ty C.WorkNever C.SingleResult C.NoUnfolding)

natLit, integerLit :: Integer -> C.Term
natLit = C.Literal . NaturalLiteral
integerLit = C.Literal . IntegerLiteral

-- | A vector of @Integer@ literals
intVec :: [Integer] -> C.Term
intVec xs = C.mkVec nilDc consDc integerPrimTy (toInteger (length xs)) (map integerLit xs)

tvN, tvA, tvD :: C.TyVar
tvN = kindedTyVar User "n" 9101 typeNatKind
tvA = kindedTyVar User "a" 9102 liftedTypeKind
tvD = kindedTyVar User "d" 9103 typeNatKind

assertSameResult :: C.Term -> C.Term -> Assertion
assertSameResult expected actual =
  assertBool ("expected:\n" <> showPpr expected <> "\nbut got:\n" <> showPpr actual)
    (sameResult expected actual)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector"
  [ testCase "maxIndex is the length minus one" $
      -- maxIndex :: forall n a. KnownNat n => Vec n a -> Int
      let n = C.VarTy tvN
          a = C.VarTy tvA
          ty = C.ForAllTy tvN $ C.ForAllTy tvA $
                 C.mkFunTy (knownNatTy n) $ C.mkFunTy (vecTy n a) hIntTy
      in assertSameResult (C.mkApps (C.Data iHashDc) [Left (C.Literal (IntLiteral 2))])
           (evalSubj (C.mkApps (prim "Clash.Sized.Vector.maxIndex" ty)
              [Right (numTy 3), Right integerPrimTy, Left (natLit 3), Left (intVec [1, 2, 3])]))

  , testCase "rotateLeftS evaluates the number of its SNat with a nested machine" $
      -- rotateLeftS :: forall n a d. KnownNat n => Vec n a -> SNat d -> Vec n a
      let n = C.VarTy tvN
          a = C.VarTy tvA
          d = C.VarTy tvD
          ty = C.ForAllTy tvN $ C.ForAllTy tvA $ C.ForAllTy tvD $
                 C.mkFunTy (knownNatTy n) $ C.mkFunTy (vecTy n a) $
                   C.mkFunTy (snatTy d) (vecTy n a)
          -- The number is only known after evaluating a let-expression.
          -- Rotating by the length gives the vector itself.
          y = C.mkLocalId naturalPrimTy (mkUnsafeSystemName "y" 8)
          snat = C.mkApps (C.Data snatDc)
                   [Right (numTy 3), Left (C.Let (C.NonRec y (natLit 3)) (C.Var y))]
      in assertSameResult (intVec [1, 2, 3])
           (evalSubj (C.mkApps (prim "Clash.Sized.Vector.rotateLeftS" ty)
              [ Right (numTy 3), Right integerPrimTy, Right (numTy 3)
              , Left (natLit 3), Left (intVec [1, 2, 3]), Left snat ]))

  ]
