{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector", on
  fixed inputs. @clash-differential prims --open@ compares the reductions with
  those of Clash Core on generated calls.
-}

{-# LANGUAGE LambdaCase #-}
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

tvN, tvA, tvD, tvP, tvK, tvL :: C.TyVar
tvN = kindedTyVar User "n" 9101 typeNatKind
tvA = kindedTyVar User "a" 9102 liftedTypeKind
tvD = kindedTyVar User "d" 9103 typeNatKind
tvP = kindedTyVar User "p" 9104 liftedTypeKind
tvK = kindedTyVar User "k" 9105 typeNatKind
tvL = kindedTyVar User "l" 9107 typeNatKind

assertSameResult :: C.Term -> C.Term -> Assertion
assertSameResult expected actual =
  assertBool ("expected:\n" <> showPpr expected <> "\nbut got:\n" <> showPpr actual)
    (sameResult expected actual)

-- | The types of the binders of the patterns in a term
patBinderTypes :: C.Term -> [C.Type]
patBinderTypes = \case
  C.Lam _ e -> patBinderTypes e
  C.TyLam _ e -> patBinderTypes e
  C.App e1 e2 -> patBinderTypes e1 <> patBinderTypes e2
  C.TyApp e _ -> patBinderTypes e
  C.Let (C.NonRec _ x) e -> patBinderTypes x <> patBinderTypes e
  C.Let (C.Rec xs) e -> concatMap (patBinderTypes . snd) xs <> patBinderTypes e
  C.Case e _ alts ->
    patBinderTypes e
      <> concat [ map C.varType ids <> patBinderTypes a | (C.DataPat _ _ ids, a) <- alts ]
      <> concat [ patBinderTypes a | (p, a) <- alts, not (isDataPat p) ]
  C.Cast e _ _ -> patBinderTypes e
  C.Tick _ e -> patBinderTypes e
  _ -> []
 where
  isDataPat C.DataPat{} = True
  isDataPat _ = False

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

  , testCase "dtfold binds the halves of the vector at their vector type" $
      -- dtfold :: forall p k a. KnownNat k => Proxy p -> (a -> p @@ 0)
      --   -> (forall l. SNat l -> p @@ l -> p @@ l -> p @@ (l + 1))
      --   -> Vec (2^k) a -> p @@ k
      --
      -- with every @p \@\@ _@ an @Int@, and @Vec k a@ for @Vec (2^k) a@: the
      -- types of arguments aren't checked. The reduction of Clash Core gives
      -- the binders of the patterns that select the halves the type
      -- @2^(k-1)@ itself.
      let p = C.VarTy tvP
          a = C.VarTy tvA
          gTy = C.ForAllTy tvL $ C.mkFunTy (snatTy (C.VarTy tvL)) $
                  C.mkFunTy hIntTy $ C.mkFunTy hIntTy hIntTy
          ty = C.ForAllTy tvP $ C.ForAllTy tvK $ C.ForAllTy tvA $
                 C.mkFunTy (knownNatTy (C.VarTy tvK)) $ C.mkFunTy p $
                   C.mkFunTy (C.mkFunTy a hIntTy) $ C.mkFunTy gTy $
                     C.mkFunTy (vecTy (C.VarTy tvK) a) hIntTy
          -- f and g apply free variables to their arguments
          fV = C.mkLocalId (C.mkFunTy integerPrimTy hIntTy) (mkUnsafeSystemName "fV" 10)
          gV = C.mkLocalId gTy (mkUnsafeSystemName "gV" 11)
          x = C.mkLocalId integerPrimTy (mkUnsafeSystemName "x" 12)
          s = C.mkLocalId (snatTy (C.VarTy tvL)) (mkUnsafeSystemName "s" 13)
          r1 = C.mkLocalId hIntTy (mkUnsafeSystemName "r1" 14)
          r2 = C.mkLocalId hIntTy (mkUnsafeSystemName "r2" 15)
          f = C.Lam x (C.App (C.Var fV) (C.Var x))
          g = C.TyLam tvL $ C.Lam s $ C.Lam r1 $ C.Lam r2 $
                C.mkApps (C.Var gV) [Right (C.VarTy tvL), Left (C.Var s), Left (C.Var r1), Left (C.Var r2)]
          r = evalSubj (C.mkApps (prim "Clash.Sized.Vector.dtfold" ty)
                [ Right hIntTy, Right (numTy 1), Right integerPrimTy
                , Left (natLit 1), Left (C.mkApps (C.Data iHashDc) [Left (C.Literal (IntLiteral 0))])
                , Left f, Left g, Left (intVec [1, 2]) ])
          binderTys = patBinderTypes r
      in do
        assertBool ("no patterns in: " <> showPpr r) (not (null binderTys))
        assertBool ("binder types: " <> show (map showPpr binderTys))
          (all (== vecTy (numTy 1) integerPrimTy) binderTys)
  ]
