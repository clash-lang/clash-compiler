{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.RTree", on
  fixed inputs. @clash-differential prims --open@ compares the reductions with
  those of Clash Core on generated calls.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.RTree (tests) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (..))
import Clash.Core.Pretty (showPpr)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Core.TysPrim (integerPrimTy, liftedTypeKind, typeNatKind)
import qualified Clash.Core.Util as C (mkRTree)

import Test.Clash.Rewrite
  ( addTy, brDc, ghcPairDc, ghcPairTy, heapLookupEvaluator, kindedTyVar, lrDc, numTy
  , rtreeTy, snatTy )
import Test.Clash.Scoped (testTypeEnv)
import Test.Clash.Scoped.Evaluator.Differential (portedEnv, sameResult, scopedWhnf)

-- | Evaluate with the ported reductions, as the subject of a case-expression
evalSubj :: C.Term -> C.Term
evalSubj = scopedWhnf (portedEnv testTypeEnv heapLookupEvaluator) mempty True

-- | A primitive with the given name and type
prim :: Text -> C.Type -> C.Term
prim nm ty = C.Prim (C.PrimInfo nm ty C.WorkNever C.SingleResult C.NoUnfolding)

-- | A tree of the given depth of @Integer@ literals
intTree :: Integer -> [Integer] -> C.Term
intTree d = C.mkRTree lrDc brDc integerPrimTy d . map (C.Literal . IntegerLiteral)

tvD, tvA :: C.TyVar
tvD = kindedTyVar User "d" 9201 typeNatKind
tvA = kindedTyVar User "a" 9202 liftedTypeKind

assertSameResult :: C.Term -> C.Term -> Assertion
assertSameResult expected actual =
  assertBool ("expected:\n" <> showPpr expected <> "\nbut got:\n" <> showPpr actual)
    (sameResult expected actual)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.RTree"
  [ testCase "tsplit gives the subtrees as a pair" $
      -- tsplit :: forall d a. RTree (d + 1) a -> (RTree d a, RTree d a)
      let d = C.VarTy tvD
          a = C.VarTy tvA
          ty = C.ForAllTy tvD $ C.ForAllTy tvA $
                 C.mkFunTy (rtreeTy (addTy d (numTy 1)) a) (ghcPairTy (rtreeTy d a) (rtreeTy d a))
          subTy = rtreeTy (numTy 0) integerPrimTy
      in assertSameResult
           (C.mkApps (C.Data ghcPairDc)
              [Right subTy, Right subTy, Left (intTree 0 [1]), Left (intTree 0 [2])])
           (evalSubj (C.mkApps (prim "Clash.Sized.RTree.tsplit" ty)
              [Right (numTy 0), Right integerPrimTy, Left (intTree 1 [1, 2])]))

  , testCase "treplicate builds a tree of copies of its argument" $
      -- treplicate :: forall d a. SNat d -> a -> RTree d a
      let d = C.VarTy tvD
          a = C.VarTy tvA
          ty = C.ForAllTy tvD $ C.ForAllTy tvA $
                 C.mkFunTy (snatTy d) $ C.mkFunTy a (rtreeTy d a)
      in assertSameResult (intTree 2 [7, 7, 7, 7])
           (evalSubj (C.mkApps (prim "Clash.Sized.RTree.treplicate" ty)
              -- The reduction doesn't look at the SNat, but it must be a value
              [ Right (numTy 2), Right integerPrimTy, Left (C.Literal (NaturalLiteral 2))
              , Left (C.Literal (IntegerLiteral 7)) ]))
  ]
