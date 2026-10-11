{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator.Primitives": the registry of ported
  reductions, examples of ported reductions on fixed inputs, the adapter's
  handling of the heap, and the harness that compares ported reductions with
  the adapter ("Test.Clash.Scoped.Evaluator").

  The reductions of Clash Core are in @clash-ghc@, so these tests use stand-in
  reductions of Clash Core. @clash-differential prims@ compares the ported
  reductions with the real ones.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Tests.Core.Scoped.Evaluator.Primitives (tests) where

import Control.Exception (evaluate)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (newIORef, readIORef)
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe (isJust)
import Test.Tasty.QuickCheck (Property, counterexample, forAll, testProperty)

import qualified Clash.Core.Evaluator.Types as CE
import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (..), mkUnsafeSystemName)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Core.TysPrim
  ( integerPrimTy, intPrimTy, liftedTypeKind, wordPrimTy )
import qualified Clash.Core.Util as C (mkVec)
import qualified Clash.Core.Var as C
import qualified Clash.Normalize.Primitives as NP

import Clash.Core.Scoped.Evaluator.Primitives (scopedPrimReductions)
import Clash.Core.Scoped.Evaluator.Primitives.Util
  (bvConPrim, intLiteral, integerToIntLiteral, liftLit2)
import Clash.Core.Scoped.Evaluator.Types (Env, PrimReduction)

import Test.Clash.Rewrite
  ( addTy, assertAlphaEq, consDc, ghcBoolTy, ghcTrueDataCon, heapLookupEvaluator
  , intId, mkTyConName, nilDc, numTy, pairDataCon, pairTy, rtreeTy, snatTy, tcApp
  , testTyConMap, vecTy )
import Test.Clash.Scoped (testTypeEnv)
import Test.Clash.Scoped.Evaluator
import Test.Clash.Scoped.Evaluator.Primitives

portedTests :: [TestTree]
portedTests =
  [ testCase "+# adds literals" $
      assertAlphaEq (intLit 5)
        (eval (C.mkApps (prim "GHC.Prim.+#" (binTy intPrimTy intPrimTy))
                 [Left (intLit 2), Left (intLit 3)]))

  , testCase "quotInt# by zero is undefined" $
      assertAlphaEq (C.TyApp (C.Prim NP.undefined) intPrimTy)
        (eval (C.mkApps (prim "GHC.Prim.quotInt#" (binTy intPrimTy intPrimTy))
                 [Left (intLit 7), Left (intLit 0)]))

  , testCase "quotRemInt# builds the constructor of its result type" $
      assertAlphaEq (C.mkApps (C.Data pairDataCon) [Left (intLit 3), Left (intLit 1)])
        (eval (C.mkApps (prim "GHC.Prim.quotRemInt#" (binTy intPrimTy pairTy))
                 [Left (intLit 7), Left (intLit 2)]))

  , testCase "andI# (liftLit2)" $
      assertAlphaEq (intLit 2)
        (eval (C.mkApps (prim "GHC.Prim.andI#" (binTy intPrimTy intPrimTy))
                 [Left (intLit 6), Left (intLit 3)]))

  , testCase "notI# (liftLit1)" $
      assertAlphaEq (intLit (-7))
        (eval (C.App (prim "GHC.Prim.notI#" (C.mkFunTy intPrimTy intPrimTy)) (intLit 6)))

  , testCase "liftLit2 doesn't reduce variables" $
      let x = C.mkLocalId intPrimTy (mkUnsafeSystemName "x" 7)
          t = C.mkApps (prim "GHC.Prim.andI#" (binTy intPrimTy intPrimTy))
                [Left (intLit 6), Left (C.Var x)]
      in assertAlphaEq t (eval t)

  , testCase "eq## of bits" $
      assertAlphaEq (C.Data ghcTrueDataCon)
        (eval (C.mkApps (prim "Clash.Sized.Internal.BitVector.eq##" (binTy bitTy ghcBoolTy))
                 [Left (bit 0 1), Left (bit 0 1)]))

  , testCase "++# of bit vectors" $
      -- (++#) :: forall m n. KnownNat m => BitVector n -> BitVector m -> BitVector (n + m)
      let appendTy =
            C.ForAllTy tvM $ C.ForAllTy tvN $
              C.mkFunTy (knownNatTy (C.VarTy tvM)) $
                C.mkFunTy (bvTy (C.VarTy tvN)) $
                  C.mkFunTy (bvTy (C.VarTy tvM)) (bvTy (addTy (C.VarTy tvN) (C.VarTy tvM)))
          -- The size of the result is the result type's, like the reduction
          -- of Clash Core gives
          expected = C.mkApps (closedPrimTerm (bvConPrim bvTcNm))
            [ Right (addTy (numTy 2) (numTy 3)), Left (natLit 5), Left (natLit 0b00001)
            , Left (integerLit 0b10011) ]
      in assertAlphaEq expected
           (eval (C.mkApps (prim "Clash.Sized.Internal.BitVector.++#" appendTy)
                    [ Right (numTy 3), Right (numTy 2), Left (natLit 3)
                    , Left (bv 2 0 0b10), Left (bv 3 0b001 0b011) ]))

  , testCase "xToBV evaluates its argument with a nested machine" $
      -- xToBV :: KnownNat n => BitVector n -> BitVector n, lazy in its argument
      let xToBVTy =
            C.ForAllTy tvN $ C.mkFunTy (knownNatTy (C.VarTy tvN)) $
              C.mkFunTy (bvTy (C.VarTy tvN)) (bvTy (C.VarTy tvN))
          y = C.mkLocalId (bvTy (numTy 2)) (mkUnsafeSystemName "y" 8)
      in assertAlphaEq (bv 2 0 1)
           (allocatedBody (evalSubj (C.mkApps (prim "Clash.Sized.Internal.BitVector.xToBV" xToBVTy)
              [ Right (numTy 2), Left (natLit 2)
              , Left (C.Let (C.NonRec y (bv 2 0 1)) (C.Var y)) ])))

  , testCase "replicate shares its argument" $
      -- replicate :: SNat n -> a -> Vec n a, lazy in its second argument
      let replicateTy =
            C.ForAllTy tvN $ C.ForAllTy tvA $
              C.mkFunTy (snatTy (C.VarTy tvN)) $ C.mkFunTy (C.VarTy tvA) (vecTy (C.VarTy tvN) (C.VarTy tvA))
          expected = C.mkVec nilDc consDc integerPrimTy 2 [integerLit 7, integerLit 7]
          r = evalSubj (C.mkApps (prim "Clash.Sized.Vector.replicate" replicateTy)
                -- The reduction doesn't look at the SNat, but it must be a value
                [Right (numTy 2), Right integerPrimTy, Left (natLit 2), Left (integerLit 7)])
      in assertBool ("unexpected result: " <> show r) (sameResult expected r)
  ]

-- * The adapter

-- | An evaluator of Clash Core with stand-in reductions:
--
-- * @plus@ adds two @Int#@ literals;
-- * @dupWhnf@ evaluates the first field of a pair with the machine of Clash
--   Core, and pairs the result with itself.
standIn :: CE.Evaluator
standIn = heapLookupEvaluator
  { CE.primStep = \tcm _isSubj p _tys args m -> case (C.primName p, args) of
      ("plus", [CE.Lit (IntLiteral i), CE.Lit (IntLiteral j)]) ->
        Just (CE.setTerm (intLit (i + j)) m)
      ("dupWhnf", [CE.DC _ (Left a : _)]) ->
        let m1 = CE.whnf heapLookupEvaluator tcm False (CE.setTerm a (CE.stackClear m))
            w = CE.mTerm m1
        in Just (CE.setTerm (C.mkApps (C.Data pairDataCon) [Left w, Left w]) m1)
      _ -> Nothing
  }

adapterTests :: [TestTree]
adapterTests =
  [ testCase "a reduction sees the heap bindings its arguments reach" $
      -- let y = 7 in dupWhnf (MkPair y y): no list says that dupWhnf needs the
      -- heap
      let y = intId User "y" 2
      in assertAlphaEq (C.mkApps (C.Data pairDataCon) [Left (intLit 7), Left (intLit 7)])
           (allocatedBody (scopedWhnf (adapterEnv testTypeEnv standIn) mempty False
             (C.Let (C.NonRec y (intLit 7))
               (C.App (prim "dupWhnf" (C.mkFunTy pairTy pairTy))
                      (C.mkApps (C.Data pairDataCon) [Left (C.Var y), Left (C.Var y)])))))
  ]

-- * The harness

-- | A port of @plus@
plusPort :: PrimReduction
plusPort = snd $ liftLit2 "plus" intLiteral (\i j -> intLit' (i + j))
 where
  intLit' = integerToIntLiteral

-- | A wrong port of @plus@
minusPort :: PrimReduction
minusPort = snd $ liftLit2 "plus" intLiteral (\i j -> integerToIntLiteral (i - j))

plusCall :: C.Term -> C.Term -> C.Term
plusCall a b = C.mkApps (prim "plus" (binTy intPrimTy intPrimTy)) [Left a, Left b]

compareWith :: PrimReduction -> C.Term -> PrimOutcome
compareWith r = comparePrimCall testTypeEnv standIn mempty "plus" r False

harnessTests :: [TestTree]
harnessTests =
  [ testCase "a correct port agrees" $
      compareWith plusPort (plusCall (intLit 2) (intLit 3)) @?= Agree
  , testCase "a wrong port differs" $
      assertBool "expected a difference" $
        case compareWith minusPort (plusCall (intLit 2) (intLit 3)) of
          Differ {} -> True
          _ -> False
  , testCase "a call that doesn't reduce is stuck on both sides" $
      compareWith plusPort (plusCall (C.Var (intId User "x" 1)) (intLit 3)) @?= BothStuck

  , testCase "recordingEnv records and checks reductions" $ do
      ref <- newIORef []
      let env :: Env
          env = recordingEnv ref True testTypeEnv standIn
          call = C.mkApps (prim "GHC.Prim.+#" (binTy intPrimTy intPrimTy))
                   [Left (intLit 2), Left (intLit 3)]
      _ <- evaluate (scopedWhnf env mempty False call)
      events <- readIORef ref
      case events of
        [PrimEvent {..}] -> do
          peName @?= "GHC.Prim.+#"
          peVia @?= Ported
          peReduced @?= True
          -- The stand-in evaluator doesn't reduce (+#)
          fmap outcomeKey peCheck @?= Just "only ported reduces"
        _ -> assertFailure ("unexpected events: " <> show (length events))

  , testProperty "generated calls have an argument for every argument type" prop_genArity
  , testProperty "calls over sized types, vectors and data types can be generated" prop_genTypes
  , testProperty "open calls have values for the arguments that can't be generated" prop_genOpen
  ]

-- | Generated calls of @Int# -> Integer -> Int#@ have two literals of the
-- right kinds
prop_genArity :: Property
prop_genArity =
  forAll (genPrimCall testTyConMap (C.PrimInfo "p" ty C.WorkNever C.SingleResult C.NoUnfolding)) $ \call ->
    counterexample (show call) $ case fmap C.collectArgs call of
      Just (_, [Left (C.Literal (IntLiteral _)), Left (C.Literal (IntegerLiteral _))]) -> True
      _ -> False
 where
  ty = C.mkFunTy intPrimTy (C.mkFunTy integerPrimTy intPrimTy)

-- | Calls of primitives over the sized types, vectors and other data types
-- can be generated
prop_genTypes :: Property
prop_genTypes =
  forAll (mapM (genPrimCall testTyConMap . mkPrim) [vecHeadTy, sizedTy, boolTy, treeTy]) $ \calls ->
    counterexample (show calls) (all isJust calls)
 where
  mkPrim t = C.PrimInfo "p" t C.WorkNever C.SingleResult C.NoUnfolding
  -- head :: forall n a. Vec (n + 1) a -> a
  vecHeadTy = C.ForAllTy tvN $ C.ForAllTy tvA $
    C.mkFunTy (vecTy (addTy (C.VarTy tvN) (numTy 1)) (C.VarTy tvA)) (C.VarTy tvA)
  -- (+#) :: forall n. KnownNat n => BitVector n -> BitVector n -> BitVector n
  sizedTy = C.ForAllTy tvN $ C.mkFunTy (knownNatTy (C.VarTy tvN)) $
    binTy (bvTy (C.VarTy tvN)) (bvTy (C.VarTy tvN))
  boolTy = C.mkFunTy wordPrimTy (C.mkFunTy ghcBoolTy (C.mkFunTy bitTy ghcBoolTy))
  -- textract :: forall a. RTree 0 a -> a
  treeTy = C.ForAllTy tvA $ C.mkFunTy (rtreeTy (numTy 0) (C.VarTy tvA)) (C.VarTy tvA)

-- | Open calls of a primitive with a dictionary, a polymorphic function and a
-- type variable of another kind than @Nat@ and @Type@ have values for them:
-- the opaque value of a type without constructors, a function, and a free
-- type variable
prop_genOpen :: Property
prop_genOpen =
  forAll (genOpenPrimCall testTyConMap prim') $ \call ->
    counterexample (show call) $ case C.collectArgs call of
      (_, [Right (C.VarTy _), Left (C.TyApp (C.Prim _) _), Left (C.TyLam _ (C.Lam _ _))]) -> True
      _ -> False
 where
  prim' = C.PrimInfo "p" ty C.WorkNever C.SingleResult C.NoUnfolding
  -- p :: forall (f :: Type -> Type). Sig f -> (forall a. a -> a) -> Int#
  tvF = C.mkTyVar (C.mkFunTy liftedTypeKind liftedTypeKind) (mkUnsafeSystemName "f" 9004)
  ty = C.ForAllTy tvF $
         C.mkFunTy (tcApp (mkTyConName "Sig" 4004) [C.VarTy tvF]) $
           C.mkFunTy (C.ForAllTy tvA (C.mkFunTy (C.VarTy tvA) (C.VarTy tvA))) intPrimTy

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator.Primitives"
  [ testCase "no primitive has two reductions" $
      () <$ evaluate (HashMap.size scopedPrimReductions)
  , testGroup "ported reductions" portedTests
  , testGroup "adapter" adapterTests
  , testGroup "harness" harnessTests
  ]
