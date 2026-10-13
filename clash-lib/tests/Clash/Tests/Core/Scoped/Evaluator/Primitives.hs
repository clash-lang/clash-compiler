{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator.Primitives": the registry of ported
  reductions, and examples of ported reductions on fixed inputs.
  @clash-differential prims@ compares the ported reductions with those of
  Clash Core.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Tests.Core.Scoped.Evaluator.Primitives (tests) where

import Control.Exception (evaluate)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Float (castDoubleToWord64)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (..))

import Clash.Core.Scoped.Alpha (aeqTy)
import Clash.Core.Scoped.Builder (TmB, TyB, appB, dataB, literalB, runClosedType)
import Clash.Core.Scoped.Evaluator.Primitives (scopedPrimReductions)
import Clash.Core.Scoped.Evaluator.Primitives.Util (bvConPrim, signedConPrim)
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
import Clash.Core.Scoped.Evaluator.Types (PrimReduction)
import Clash.Core.Scoped.Syntax (S (VoidS), Term (..), emptyScope)
import Clash.Core.Scoped.Term (collectArgs)

import Test.Clash.Rewrite
  ( doubleDc, doubleTcNm, eqDc, ghcFalseDataCon, ghcTrueDataCon, gtDc, ltDc, mkTyConName
  , orderingTcNm, pairDataCon, proxyDc, showPprU, signedTyConName, someNatDc, someNatTcNm
  , t2DataCon, t2TyConName, t3DataCon, t3TyConName )
import Test.Clash.Scoped (assertTermAlphaEq, assertTermIs, builtTerm)
import Test.Clash.Scoped.Evaluator (allocatedBody, assertSameValue)
import Test.Clash.Scoped.Evaluator.Primitives

import qualified Clash.Annotations.BitRepresentation.Deriving
import qualified Clash.Promoted.Nat
import qualified GHC.Base
import qualified GHC.Num.Integer
import qualified GHC.TypeLits
import qualified GHC.TypeNats
import qualified Language.Haskell.TH as TH

pairTy, doubleTy, orderingTy, someNatTy :: TyB
pairTy = ty "Pair"
doubleTy = tc doubleTcNm []
orderingTy = tc orderingTcNm []
someNatTy = tc someNatTcNm []

t2Ty :: TyB -> TyB -> TyB
t2Ty a b = tc t2TyConName [a, b]

t3Ty :: TyB -> TyB -> TyB -> TyB
t3Ty a b c = tc t3TyConName [a, b, c]

portedTests :: [TestTree]
portedTests =
  [ testCase "+# adds literals" $
      assertEvalsTo (intLit 5)
        (apps (prim "GHC.Prim.+#" (binTy intPrimTy intPrimTy)) [Left (intLit 2), Left (intLit 3)])

  , testCase "quotInt# by zero is undefined" $
      assertEvalsTo (undefinedAt intPrimTy)
        (apps (prim "GHC.Prim.quotInt#" (binTy intPrimTy intPrimTy)) [Left (intLit 7), Left (intLit 0)])

  , testCase "quotRemInt# builds the constructor of its result type" $
      assertEvalsTo (apps (dataB pairDataCon) [Left (intLit 3), Left (intLit 1)])
        (apps (prim "GHC.Prim.quotRemInt#" (binTy intPrimTy pairTy)) [Left (intLit 7), Left (intLit 2)])

  , testCase "andI# (liftLit2)" $
      assertEvalsTo (intLit 2)
        (apps (prim "GHC.Prim.andI#" (binTy intPrimTy intPrimTy)) [Left (intLit 6), Left (intLit 3)])

  , testCase "notI# (liftLit1)" $
      assertEvalsTo (intLit (-7))
        (apps (prim "GHC.Prim.notI#" (intPrimTy ~> intPrimTy)) [Left (intLit 6)])

  , testCase "liftLit2 doesn't reduce variables" $
      let t = apps (prim "GHC.Prim.andI#" (binTy intPrimTy intPrimTy)) [Left (intLit 6), Left (var "x_7")]
      in evalOpen [("x_7", intPrimTy)] False t $ \ts r -> assertTermIs ts (builtTerm t) r

  , testCase "eq## of bits" $
      assertEvalsTo (dataB ghcTrueDataCon)
        (apps (prim "Clash.Sized.Internal.BitVector.eq##" (binTy bitTy ghcBoolTy))
           [Left (bit 0 1), Left (bit 0 1)])

  , testCase "++# of bit vectors" $
      -- (++#) :: forall m n. KnownNat m => BitVector n -> BitVector m -> BitVector (n + m)
      let appendTy =
            forAll "m_9002" natKind $ \m -> forAll "n_9001" natKind $ \n ->
              knownNatTy m ~> bvTy n ~> bvTy m ~> bvTy (addTy n m)
          -- The size of the result is the result type's, like the reduction
          -- of Clash Core gives
          expected = apps (conPrim (bvConPrim bvTcNm))
            [ Right (addTy (numTy 2) (numTy 3)), Left (natLit 5), Left (natLit 0b00001)
            , Left (integerLit 0b10011) ]
      in assertEvalsTo expected
           (apps (prim "Clash.Sized.Internal.BitVector.++#" appendTy)
              [ Right (numTy 3), Right (numTy 2), Left (natLit 3)
              , Left (bv 2 0 0b10), Left (bv 3 0b001 0b011) ])

  , testCase "xToBV evaluates its argument with a nested machine" $
      -- xToBV :: KnownNat n => BitVector n -> BitVector n, lazy in its argument
      let xToBVTy = forAll "n_9001" natKind $ \n -> knownNatTy n ~> bvTy n ~> bvTy n
      in assertTermAlphaEq (closed (bv 2 0 1))
           (allocatedBody (evalSubj (apps (prim "Clash.Sized.Internal.BitVector.xToBV" xToBVTy)
              [ Right (numTy 2), Left (natLit 2)
              , Left (letNonRec "y_8" (bvTy (numTy 2)) (bv 2 0 1) id) ])))

  , testCase "replicate shares its argument" $
      -- replicate :: SNat n -> a -> Vec n a, lazy in its second argument
      let replicateTy =
            forAll "n_9001" natKind $ \n -> forAll "a_9003" typeKind $ \a ->
              snatTy n ~> a ~> vecTy n a
      in assertSameValue emptyScope (closed (vec integerPrimTy [integerLit 7, integerLit 7]))
           (evalSubj (apps (prim "Clash.Sized.Vector.replicate" replicateTy)
              -- The reduction doesn't look at the SNat, but it must be a value
              [Right (numTy 2), Right integerPrimTy, Left (natLit 2), Left (integerLit 7)]))
  ]

-- * Results built from types

signedTy :: TyB -> TyB
signedTy n = tc signedTyConName [n]

signed :: Integer -> Integer -> TmB
signed n val =
  apps (conPrim (signedConPrim signedTyConName)) [Right (numTy n), Left (natLit n), Left (integerLit val)]

doubleLit :: Double -> TmB
doubleLit = literalB . DoubleLiteral . castDoubleToWord64

-- | The name of a primitive, as Clash names it
nameText :: TH.Name -> Text
nameText = Text.pack . show

-- | Evaluate with the given reductions, and nothing for the other primitives
evalReductions :: [(Text, PrimReduction)] -> Bool -> TmB -> Term 'VoidS
evalReductions rs = evalEnvIn (primEnv sizedTypeEnv rs)

-- | Reductions whose results are built from the type of their primitive, or
-- from the types of its arguments
resultTypeTests :: [TestTree]
resultTypeTests =
  [ testCase "$wproperFractionDouble builds its result from the instantiated type" $
      -- forall b. Integral b => Double# -> (# b, Double #)
      let integralTy b = tc (mkTyConName "GHC.Real.Integral" 4005) [b]
          pfTy = forAll "a_9003" typeKind $ \b -> integralTy b ~> doublePrimTy ~> t2Ty b doubleTy
          call nm = apps (prim nm pfTy)
            [Right (signedTy (numTy 8)), Left (natLit 0), Left (doubleLit (-2.5))]
          -- The reduction of Clash Core gives the type arguments of the type of
          -- the primitive, @b@ and @Double@
          expected = apps (dataB t2DataCon)
            [ Right (signedTy (numTy 8)), Right doubleTy
            , Left (signed 8 (-2)), Left (appB (dataB doubleDc) (doubleLit (-0.5))) ]
          floatPrims = PFloat.primitives ++ PInternalFloat.primitives
      in mapM_ (\nm -> assertTermAlphaEq (closed expected) (evalReductions floatPrims False (call nm)))
           ["GHC.Float.$wproperFractionDouble", "GHC.Internal.Float.$wproperFractionDouble"]

  , testCase "integerDecodeDouble# builds the constructor of its result type" $
      assertTermAlphaEq
        (closed (apps (dataB t2DataCon)
          [ Right integerPrimTy, Right intPrimTy
          , Left (integerLit 4503599627370496), Left (intLit (-53)) ]))
        (evalReductions PInteger.primitives False
          (apps (prim (nameText 'GHC.Num.Integer.integerDecodeDouble#) (doublePrimTy ~> t2Ty integerPrimTy intPrimTy))
             [Left (doubleLit 0.5)]))

  , testCase "$wunpackCStringAscii# builds the constructor of its result type" $
      let r = evalReductions PShow.primitives False
                (apps (prim "Data.Text.Show.$wunpackCStringAscii#"
                         (stringPrimTy ~> t3Ty byteArrayPrimTy intPrimTy intPrimTy))
                   [Left (literalB (StringLiteral "abc"))])
          tyIs t = aeqTy (runClosedType t)
      in case collectArgs r of
           ( Data dc
             , [ Right a, Right b, Right c
               , Left (Literal (ByteArrayLiteral _)), Left off, Left len ] )
             | dc == t3DataCon
             -> do assertBool "the type arguments are ByteArray#, Int# and Int#"
                     (tyIs byteArrayPrimTy a && tyIs intPrimTy b && tyIs intPrimTy c)
                   assertTermAlphaEq (closed (intLit 0)) off
                   assertTermAlphaEq (closed (intLit 3)) len
           _ -> assertFailure ("unexpected result: " <> showPprU r)

  , testCase "integerCompare gives a constructor of Ordering" $ do
      let cmp i j = evalReductions PInteger.primitives False
            (apps (prim (nameText 'GHC.Num.Integer.integerCompare) (binTy integerPrimTy orderingTy))
               [Left (integerLit i), Left (integerLit j)])
      assertTermAlphaEq (closed (dataB gtDc)) (cmp 3 2)
      assertTermAlphaEq (closed (dataB eqDc)) (cmp 2 2)
      assertTermAlphaEq (closed (dataB ltDc)) (cmp (-1) 2)

  , testCase "powSNat gives an SNat of the power" $
      -- powSNat :: forall a b. SNat a -> SNat b -> SNat (a^b)
      let powTy = forAll "n_9001" natKind $ \n -> forAll "b_9004" natKind $ \b ->
            snatTy n ~> snatTy b ~> snatTy n
      in assertTermAlphaEq (closed (snat 81))
           (evalReductions PNat.primitives False
              (apps (prim (nameText 'Clash.Promoted.Nat.powSNat) powTy)
                 [Right (numTy 3), Right (numTy 4), Left (snat 3), Left (snat 4)]))

  , testCase "withSomeSNat applies its function to an SNat" $
      -- withSomeSNat :: forall r. Natural -> (forall n. SNat n -> r) -> r
      let withTy = forAll "a_9003" typeKind $ \r ->
            naturalPrimTy ~> forAll "n_9001" natKind (\n -> snatTy n ~> r) ~> r
          -- /\n. \(s :: SNat n) -> MkPair s s: a constructor of a pair that
          -- keeps the SNat
          fun = tyLam "n_9001" natKind $ \n -> lam "s_10" (snatTy n) $ \s ->
            apps (dataB pairDataCon) [Left s, Left s]
      -- 'GHC.TypeNats.SNat' is a newtype over 'Natural', so the SNat is the
      -- literal it wraps
      in assertSameValue emptyScope (closed (apps (dataB pairDataCon) [Left (natLit 5), Left (natLit 5)]))
           (evalReductions PTypeNats.primitives False
              (apps (prim (nameText 'GHC.TypeNats.withSomeSNat) withTy)
                 [Right pairTy, Left (natLit 5), Left fun]))

  , testCase "someNatVal hides a natural in SomeNat" $
      let expected = apps (dataB someNatDc)
            [ Right (numTy 5), Left (natLit 5)
            , Left (apps (dataB proxyDc) [Right natKind, Right (numTy 5)]) ]
          call nm = apps (prim nm (naturalPrimTy ~> someNatTy)) [Left (natLit 5)]
      in do assertTermAlphaEq (closed expected)
              (evalReductions PTypeNats.primitives False (call (nameText 'GHC.TypeNats.someNatVal)))
            assertTermAlphaEq (closed expected)
              (evalReductions PTypeLits.primitives False (call (nameText 'GHC.TypeLits.someNatVal)))

  , testCase "dontApplyInHDL applies its function as the subject of a case" $
      let applyTy = forAll "a_9003" typeKind $ \_ -> forAll "n_9001" natKind $ \_ ->
            (intPrimTy ~> intPrimTy) ~> intPrimTy ~> intPrimTy
          call = apps (prim (nameText 'Clash.Annotations.BitRepresentation.Deriving.dontApplyInHDL) applyTy)
            [ Right intPrimTy, Right intPrimTy
            , Left (lam "x_11" intPrimTy $ \x ->
                apps (prim "GHC.Prim.+#" (binTy intPrimTy intPrimTy)) [Left x, Left (intLit 1)])
            , Left (intLit 5) ]
      in assertTermAlphaEq (closed (intLit 6))
           (allocatedBody (evalReductions (PDeriving.primitives ++ PPrim.primitives) True call))

  , testCase "eqString compares string literals" $
      let stringTy = tc (mkTyConName "GHC.Types.List" 4006) [charPrimTy]
          str = appB (prim "GHC.CString.unpackCString#" (stringPrimTy ~> stringTy))
                  . literalB . StringLiteral
          eqStr a b = evalReductions PBase.primitives False
            (apps (prim (nameText 'GHC.Base.eqString) (binTy stringTy ghcBoolTy))
               [Left (str a), Left (str b)])
      in do assertTermAlphaEq (closed (dataB ghcTrueDataCon)) (eqStr "ab" "ab")
            assertTermAlphaEq (closed (dataB ghcFalseDataCon)) (eqStr "ab" "ba")
  ]

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator.Primitives"
  [ testCase "no primitive has two reductions" $
      () <$ evaluate (HashMap.size scopedPrimReductions)
  , testGroup "ported reductions" portedTests
  , testGroup "results built from types" resultTypeTests
  ]
