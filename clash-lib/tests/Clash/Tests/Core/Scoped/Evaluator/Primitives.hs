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
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Tests.Core.Scoped.Evaluator.Primitives (tests) where

import Control.Exception (evaluate)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Float (castDoubleToWord64)
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe (isJust)
import Test.Tasty.QuickCheck (Property, counterexample, forAll, testProperty)

import qualified Clash.Core.Evaluator.Types as CE
import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (..), mkUnsafeSystemName)
import qualified Clash.Core.Term as C
import Clash.Core.TyCon (TyConName)
import qualified Clash.Core.Type as C
import Clash.Core.TysPrim
  ( byteArrayPrimTy, charPrimTy, doublePrimTy, integerPrimTy, intPrimTy, naturalPrimTy
  , liftedTypeKind, stringPrimTy, typeNatKind, wordPrimTy )
import qualified Clash.Core.Util as C (mkVec)
import qualified Clash.Core.Var as C
import qualified Clash.Normalize.Primitives as NP

import Clash.Core.Scoped.Evaluator.Primitives (scopedPrimReductions)
import Clash.Core.Scoped.Evaluator.Primitives.Util
  (bvConPrim, intLiteral, integerToIntLiteral, liftLit2, signedConPrim)
import Test.Clash.Scoped.Evaluator.Adapter (adaptPrim)
import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Annotations.BitRepresentation.Deriving as PDeriving
import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Promoted.Nat as PNat
import qualified Clash.Core.Scoped.Evaluator.Primitives.Data.Text.Show as PShow
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Base as PBase
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Float as PFloat
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Internal.Float as PInternalFloat
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Num.Integer as PInteger
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Prim as PPrim
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.TypeLits as PTypeLits
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.TypeNats as PTypeNats
import Clash.Core.Scoped.Evaluator.Types (Env (..), PrimReduction, PrimReductions (..))

import Test.Clash.Rewrite
  ( addTy, assertAlphaEq, consDc, doubleDc, doubleTy, eqDc, ghcBoolTy, ghcFalseDataCon
  , ghcTrueDataCon, gtDc, heapLookupEvaluator, intId, ltDc, mkTyConName, nilDc, numTy
  , orderingTy, pairDataCon, pairTy, proxyDc, rtreeTy, signedTyConName, snatDc, snatTy
  , someNatDc, someNatTy, t2DataCon, t2Ty, t3DataCon, t3Ty, tcApp, testTyConMap, vecTy )
import Test.Clash.Scoped (testTypeEnv)
import Test.Clash.Scoped.Evaluator.Differential
import Test.Clash.Scoped.Evaluator.Primitives

import qualified Clash.Annotations.BitRepresentation.Deriving
import qualified Clash.Promoted.Nat
import qualified GHC.Base
import qualified GHC.Num.Integer
import qualified GHC.TypeLits
import qualified GHC.TypeNats
import qualified Language.Haskell.TH as TH

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

-- * Results built from types

signedTcNm :: TyConName
signedTcNm = signedTyConName

signedTy :: C.Type -> C.Type
signedTy n = tcApp signedTcNm [n]

signed :: Integer -> Integer -> C.Term
signed n val =
  C.mkApps (closedPrimTerm (signedConPrim signedTcNm))
    [Right (numTy n), Left (natLit n), Left (integerLit val)]

snat :: Integer -> C.Term
snat n = C.mkApps (C.Data snatDc) [Right (numTy n), Left (natLit n)]

doubleLit :: Double -> C.Term
doubleLit = C.Literal . DoubleLiteral . castDoubleToWord64

-- | The name of a primitive, as Clash names it
nameText :: TH.Name -> Text
nameText = Text.pack . show

-- | The given reductions, and nothing for the other primitives
evalReductions :: [(Text, PrimReduction)] -> Bool -> C.Term -> C.Term
evalReductions rs =
  scopedWhnf (Env testTypeEnv (PrimReductions (HashMap.fromList rs) (Just (adaptPrim heapLookupEvaluator)))) mempty

-- | Reductions whose results are built from the type of their primitive, or
-- from the types of its arguments
resultTypeTests :: [TestTree]
resultTypeTests =
  [ testCase "$wproperFractionDouble builds its result from the instantiated type" $
      -- forall b. Integral b => Double# -> (# b, Double #)
      let integralTy b = tcApp (mkTyConName "GHC.Real.Integral" 4005) [b]
          pfTy = C.ForAllTy tvA $
            C.mkFunTy (integralTy (C.VarTy tvA)) $
              C.mkFunTy doublePrimTy (t2Ty (C.VarTy tvA) doubleTy)
          call nm = C.mkApps (prim nm pfTy)
            [Right (signedTy (numTy 8)), Left (natLit 0), Left (doubleLit (-2.5))]
          -- The reduction of Clash Core gives the type arguments of the type of
          -- the primitive, @b@ and @Double@
          expected = C.mkApps (C.Data t2DataCon)
            [ Right (signedTy (numTy 8)), Right doubleTy
            , Left (signed 8 (-2)), Left (C.App (C.Data doubleDc) (doubleLit (-0.5))) ]
          floatPrims = PFloat.primitives ++ PInternalFloat.primitives
      in mapM_ (\nm -> assertAlphaEq expected (evalReductions floatPrims False (call nm)))
           ["GHC.Float.$wproperFractionDouble", "GHC.Internal.Float.$wproperFractionDouble"]

  , testCase "integerDecodeDouble# builds the constructor of its result type" $
      assertAlphaEq
        (C.mkApps (C.Data t2DataCon)
          [ Right integerPrimTy, Right intPrimTy
          , Left (integerLit 4503599627370496), Left (intLit (-53)) ])
        (evalReductions PInteger.primitives False (C.App (prim (nameText 'GHC.Num.Integer.integerDecodeDouble#)
                        (C.mkFunTy doublePrimTy (t2Ty integerPrimTy intPrimTy)))
                     (doubleLit 0.5)))

  , testCase "$wunpackCStringAscii# builds the constructor of its result type" $
      let r = evalReductions PShow.primitives False (C.App (prim "Data.Text.Show.$wunpackCStringAscii#"
                            (C.mkFunTy stringPrimTy (t3Ty byteArrayPrimTy intPrimTy intPrimTy)))
                         (C.Literal (StringLiteral "abc")))
      in case C.collectArgs r of
           ( C.Data dc
             , [ Right a, Right b, Right c
               , Left (C.Literal (ByteArrayLiteral _)), Left off, Left len ] )
             | dc == t3DataCon
             -> do mapM_ (uncurry assertAlphaEq)
                     [(byteArrayPrimTy, a), (intPrimTy, b), (intPrimTy, c)]
                   assertAlphaEq (intLit 0) off
                   assertAlphaEq (intLit 3) len
           _ -> assertFailure ("unexpected result: " <> show r)

  , testCase "integerCompare gives a constructor of Ordering" $ do
      let cmp i j = evalReductions PInteger.primitives False (C.mkApps (prim (nameText 'GHC.Num.Integer.integerCompare)
                                     (binTy integerPrimTy orderingTy))
                                [Left (integerLit i), Left (integerLit j)])
      assertAlphaEq (C.Data gtDc) (cmp 3 2)
      assertAlphaEq (C.Data eqDc) (cmp 2 2)
      assertAlphaEq (C.Data ltDc) (cmp (-1) 2)

  , testCase "powSNat gives an SNat of the power" $
      -- powSNat :: forall a b. SNat a -> SNat b -> SNat (a^b)
      let tvB = C.mkTyVar typeNatKind (mkUnsafeSystemName "b" 9004)
          powTy = C.ForAllTy tvN $ C.ForAllTy tvB $
            C.mkFunTy (snatTy (C.VarTy tvN)) $
              C.mkFunTy (snatTy (C.VarTy tvB)) (snatTy (C.VarTy tvN))
      in assertAlphaEq (snat 81)
           (evalReductions PNat.primitives False (C.mkApps (prim (nameText 'Clash.Promoted.Nat.powSNat) powTy)
                    [Right (numTy 3), Right (numTy 4), Left (snat 3), Left (snat 4)]))

  , testCase "withSomeSNat applies its function to an SNat" $
      -- withSomeSNat :: forall r. Natural -> (forall n. SNat n -> r) -> r
      let s = C.mkLocalId (snatTy (C.VarTy tvN)) (mkUnsafeSystemName "s" 10)
          withTy = C.ForAllTy tvA $
            C.mkFunTy naturalPrimTy $
              C.mkFunTy (C.ForAllTy tvN (C.mkFunTy (snatTy (C.VarTy tvN)) (C.VarTy tvA)))
                (C.VarTy tvA)
          -- /\n. \(s :: SNat n) -> case s of SNat k -> integerFromNatural k,
          -- written as a constructor of a pair that keeps the SNat
          fun = C.TyLam tvN (C.Lam s (C.mkApps (C.Data pairDataCon) [Left (C.Var s), Left (C.Var s)]))
      in assertBool "expected the function applied to SNat 5" $
           sameResult (C.mkApps (C.Data pairDataCon) [Left (snat 5), Left (snat 5)])
             (evalReductions PTypeNats.primitives False (C.mkApps (prim (nameText 'GHC.TypeNats.withSomeSNat) withTy)
                      [Right pairTy, Left (natLit 5), Left fun]))

  , testCase "someNatVal hides a natural in SomeNat" $
      let expected = C.mkApps (C.Data someNatDc)
            [ Right (numTy 5), Left (natLit 5)
            , Left (C.mkApps (C.Data proxyDc) [Right typeNatKind, Right (numTy 5)]) ]
          call nm = C.App (prim nm (C.mkFunTy naturalPrimTy someNatTy)) (natLit 5)
      in do assertAlphaEq expected
              (evalReductions PTypeNats.primitives False (call (nameText 'GHC.TypeNats.someNatVal)))
            assertAlphaEq expected
              (evalReductions PTypeLits.primitives False (call (nameText 'GHC.TypeLits.someNatVal)))

  , testCase "dontApplyInHDL applies its function as the subject of a case" $
      let x = C.mkLocalId intPrimTy (mkUnsafeSystemName "x" 11)
          applyTy = C.ForAllTy tvA $ C.ForAllTy tvN $
            C.mkFunTy (C.mkFunTy intPrimTy intPrimTy) (C.mkFunTy intPrimTy intPrimTy)
          call = C.mkApps (prim (nameText 'Clash.Annotations.BitRepresentation.Deriving.dontApplyInHDL) applyTy)
            [ Right intPrimTy, Right intPrimTy
            , Left (C.Lam x (C.mkApps (prim "GHC.Prim.+#" (binTy intPrimTy intPrimTy))
                                [Left (C.Var x), Left (intLit 1)]))
            , Left (intLit 5) ]
      in assertAlphaEq (intLit 6) (allocatedBody (evalReductions (PDeriving.primitives ++ PPrim.primitives) True call))

  , testCase "eqString compares string literals" $
      let str = C.App (prim "GHC.CString.unpackCString#" (C.mkFunTy stringPrimTy stringTy))
                      . C.Literal . StringLiteral
          stringTy = tcApp (mkTyConName "GHC.Types.List" 4006) [charPrimTy]
          eqStr a b = evalReductions PBase.primitives False (C.mkApps (prim (nameText 'GHC.Base.eqString) (binTy stringTy ghcBoolTy))
                              [Left (str a), Left (str b)])
      in do assertAlphaEq (C.Data ghcTrueDataCon) (eqStr "ab" "ab")
            assertAlphaEq (C.Data ghcFalseDataCon) (eqStr "ab" "ba")
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
  , testGroup "results built from types" resultTypeTests
  , testGroup "adapter" adapterTests
  , testGroup "harness" harnessTests
  ]
