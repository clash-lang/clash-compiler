{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator.Primitives.GHC.Prim" on fixed inputs:
  reductions with unboxed tuples as results, and the reductions that
  combinators build.

  The unboxed tuples of "GHC.Prim" are 'T2' here: the reductions only look at
  the only constructor of the result type.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Evaluator.Primitives.GHC.Prim (tests) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon)
import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Term as C
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Core.Type as C
import Clash.Core.TysPrim
  (intPrimTy, wordPrimTy)
import qualified Clash.Data.UniqMap as UniqMap
import qualified Clash.Normalize.Primitives as NP

import Test.Clash.Rewrite
  (assertAlphaEq, heapLookupEvaluator, t2TyConName, tcApp, testTyConMap)
import Test.Clash.Scoped (testTypeEnv)
import Test.Clash.Scoped.Evaluator (portedEnv, scopedWhnf)

-- | Evaluate with the ported reductions, as the subject of a case-expression
eval :: C.Term -> C.Term
eval = allocatedBody . scopedWhnf (portedEnv testTypeEnv heapLookupEvaluator) mempty True
 where
  allocatedBody = \case
    C.Letrec _ e -> e
    e -> e

-- | A primitive with the given name and type
prim :: Text -> C.Type -> C.Term
prim nm ty = C.Prim (C.PrimInfo nm ty C.WorkNever C.SingleResult C.NoUnfolding)

intLit, wordLit :: Integer -> C.Term
intLit = C.Literal . IntLiteral
wordLit = C.Literal . WordLiteral

(~>) :: C.Type -> C.Type -> C.Type
(~>) = C.mkFunTy
infixr 5 ~>

-- * Unboxed tuples

t2Ty :: C.Type -> C.Type -> C.Type
t2Ty a b = tcApp t2TyConName [a, b]

-- | The constructor of 'T2'
t2Dc :: DataCon
t2Dc = case UniqMap.lookup t2TyConName testTyConMap of
  Just tc | [dc] <- tyConDataCons tc -> dc
  _ -> error "T2 is not in testTyConMap"

-- * Other reductions

otherTests :: [TestTree]
otherTests =
  [ testCase "addIntC# builds the constructor of its result type" $
      assertAlphaEq
        (C.mkApps (C.Data t2Dc)
           [Right intPrimTy, Right intPrimTy, Left (intLit (-2 ^ (63 :: Int))), Left (intLit 1)])
        (eval (C.mkApps (prim "GHC.Prim.addIntC#" (intPrimTy ~> intPrimTy ~> t2Ty intPrimTy intPrimTy))
                 [Left (intLit (2 ^ (63 :: Int) - 1)), Left (intLit 1)]))

  , testCase "quotRemWord# by zero is undefined in both fields" $
      let undef = C.TyApp (C.Prim NP.undefined) (t2Ty wordPrimTy wordPrimTy)
      in assertAlphaEq
           (C.mkApps (C.Data t2Dc) [Right wordPrimTy, Right wordPrimTy, Left undef, Left undef])
           (eval (C.mkApps (prim "GHC.Prim.quotRemWord#" (wordPrimTy ~> wordPrimTy ~> t2Ty wordPrimTy wordPrimTy))
                    [Left (wordLit 7), Left (wordLit 0)]))

  , testCase "quotRemWord2# divides a two-word number" $
      -- 2^64 `quotRem` 3
      assertAlphaEq
        (C.mkApps (C.Data t2Dc)
           [Right wordPrimTy, Right wordPrimTy, Left (wordLit 0x5555555555555555), Left (wordLit 1)])
        (eval (C.mkApps (prim "GHC.Prim.quotRemWord2#" (wordPrimTy ~> wordPrimTy ~> wordPrimTy ~> t2Ty wordPrimTy wordPrimTy))
                 [Left (wordLit 1), Left (wordLit 0), Left (wordLit 3)]))

  , testCase "quotWord# by zero is undefined (liftLit2)" $
      assertAlphaEq (C.TyApp (C.Prim NP.undefined) wordPrimTy)
        (eval (C.mkApps (prim "GHC.Prim.quotWord#" (wordPrimTy ~> wordPrimTy ~> wordPrimTy))
                 [Left (wordLit 7), Left (wordLit 0)]))

  , testCase "popCnt# (liftLit1)" $
      assertAlphaEq (wordLit 3)
        (eval (C.App (prim "GHC.Prim.popCnt#" (wordPrimTy ~> wordPrimTy)) (wordLit 0b1011)))

  , testCase "narrow8Int#" $
      assertAlphaEq (intLit 44)
        (eval (C.App (prim "GHC.Prim.narrow8Int#" (intPrimTy ~> intPrimTy)) (intLit 300)))
  ]

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator.Primitives.GHC.Prim"
  [ testGroup "other reductions" otherTests
  ]
