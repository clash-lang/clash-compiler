{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator": the whnf evaluator of scoped Clash
  Core, and its ported primitive reductions.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Evaluator (tests) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (..), mkUnsafeSystemName)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Core.TysPrim (intPrimTy, integerPrimTy, typeNatKind)
import qualified Clash.Core.Var as C
import Clash.Driver.Types (BindingMap)

import Clash.Core.Scoped.Evaluator.Primitives (scopedPrimReductions)
import Clash.Core.Scoped.Evaluator.Types (Env (..), PrimReductions (..))

import Test.Clash.Rewrite
  ( assertAlphaEq, consDc, ghcBoolTy, ghcFalseDataCon, ghcTrueDataCon
  , globalId, intId, intLit, intTy, justDataCon
  , maybeTy, mkBindingMap, nilDc, numTy, pairDataCon, tyVar
  , vecTy )
import Test.Clash.Scoped (scopedWhnf, testTypeEnv)

-- | Evaluate with the test types, no globals, and no primitives of Clash Core
eval :: C.Term -> C.Term
eval = evalWith mempty False

-- | Evaluate as the subject of a case-expression
evalSubj :: C.Term -> C.Term
evalSubj = evalWith mempty True

-- | Evaluate with the ported primitive reductions
evalWith :: BindingMap -> Bool -> C.Term -> C.Term
evalWith = scopedWhnf (Env testTypeEnv (PrimReductions scopedPrimReductions Nothing))

-- | The body of the bindings the evaluator allocated
allocatedBody :: C.Term -> C.Term
allocatedBody = \case
  C.Letrec _ e -> e
  e -> e

x, y, z :: C.Id
x = intId User "x" 1
y = intId User "y" 2
z = intId User "z" 3

-- | A heap binder of an @Int#@ literal, for comparing up to
-- alpha-equivalence
h1 :: C.Id
h1 = C.mkLocalId intPrimTy (mkUnsafeSystemName "h" 11)

machineTests :: [TestTree]
machineTests =
  [ testCase "beta reduction allocates the argument" $
      -- (\x -> x) 3
      assertAlphaEq (C.Letrec [(h1, intLit 3)] (intLit 3))
        (eval (C.App (C.Lam x (C.Var x)) (intLit 3)))

  , testCase "let-bindings are allocated" $
      assertAlphaEq (C.Letrec [(x, intLit 1)] (intLit 1))
        (eval (C.Let (C.NonRec x (intLit 1)) (C.Var x)))

  , testCase "a self-referencing binding doesn't loop" $
      case eval (C.Letrec [(x, C.Var x)] (C.Var x)) of
        C.Letrec [_] _ -> pure ()
        r -> assertFailure ("unexpected result: " <> show r)

  , testCase "case of a known constructor substitutes its fields" $
      -- case MkPair 1 2 of MkPair y z -> z
      assertAlphaEq (intLit 2)
        (eval (C.Case (C.mkApps (C.Data pairDataCon) [Left (intLit 1), Left (intLit 2)]) intTy
                 [(C.DataPat pairDataCon [] [y, z], C.Var z)]))

  , testCase "case of a literal selects its alternative" $
      assertAlphaEq (intLit 20)
        (eval (C.Case (intLit 2) intTy
                 [ (C.DefaultPat, intLit 30)
                 , (C.LitPat (IntLiteral 1), intLit 10)
                 , (C.LitPat (IntLiteral 2), intLit 20) ]))

  , testCase "a free variable is a value" $
      assertAlphaEq (C.Var x) (eval (C.Var x))

  , testCase "globals are looked up in the global heap" $
      assertAlphaEq (intLit 42)
        (evalWith (mkBindingMap [(g, intLit 42)]) False (C.Var g))

    -- A local and a global with the same unique (decision D5 of the port)
  , testCase "locals are not looked up in the global heap" $
      assertAlphaEq (C.Var xG)
        (evalWith (mkBindingMap [(g, intLit 42)]) False (C.Var xG))

    -- Like the machine of Clash Core, it doesn't expand a partial
    -- application at the top: there is nothing to apply the abstraction to.
  , testCase "a partially applied constructor at the top is not eta-expanded" $
      assertAlphaEq (C.TyApp (C.Data justDataCon) intTy)
        (eval (C.TyApp (C.Data justDataCon) intTy))

  , testCase "partially applied constructors are eta-expanded" $
      -- (\f -> f 1) (Just @Int)
      let fL = C.mkLocalId (C.mkFunTy intTy (maybeTy intTy)) (mkUnsafeSystemName "f" 9)
          r = eval (C.App (C.Lam fL (C.App (C.Var fL) (intLit 1)))
                          (C.TyApp (C.Data justDataCon) intTy))
          expanded = C.Lam x (C.mkApps (C.Data justDataCon) [Right intTy, Left (C.Var x)])
      in case r of
           C.Letrec bs (C.App (C.TyApp (C.Data dc) _) (C.Var a))
             | dc == justDataCon
             , Just arg <- lookup a bs
             -> do
               assertAlphaEq (intLit 1) arg
               assertBool ("the expansion is not bound: " <> show r)
                 (any ((== expanded) . snd) bs)
           _ -> assertFailure ("unexpected result: " <> show r)

  , testCase "a binding the machine allocates is fresh for the free variables" $
      -- (\y -> MkPair y x) (f 1), where the argument is allocated: its binder
      -- must not be x
      case eval (C.App (C.Lam y (C.mkApps (C.Data pairDataCon) [Left (C.Var y), Left (C.Var x)]))
                       (C.App (C.Var f) (intLit 1))) of
        C.Letrec [(h, _)] _ ->
          assertBool "allocated binder clashes with a free variable" (C.varUniq h /= C.varUniq x)
        r -> assertFailure ("unexpected result: " <> show r)
  ]
 where
  g = globalId User "g" 4 intTy
  xG = intId User "x" 4
  f = globalId User "f" 5 (C.mkFunTy intTy intTy)

-- | The tests of @tests/shouldwork/Evaluator@ that test the machine, on
-- scoped Core
shouldworkTests :: [TestTree]
shouldworkTests =
  -- GlobalLocalHeapLookup: a global and a local share a unique
  [ testCase "a global is not looked up in the local heap" $
      -- let fLocal = 2 in f, where f = 1
      assertAlphaEq (intLit 1)
        (allocatedBody (evalWith (mkBindingMap [(fG, intLit 1)]) False
          (C.Let (C.NonRec fL (intLit 2)) (C.Var fG))))

  , testCase "a global argument is not the local with its unique" $
      -- let fLocal = 2 in h f
      assertAlphaEq (C.App (C.Var hL) (C.Var fG))
        (allocatedBody (evalWith (mkBindingMap [(fG, intLit 1)]) False
          (C.Let (C.NonRec fL (intLit 2)) (C.App (C.Var hL) (C.Var fG)))))

    -- LetAllocationFreshness: a let-binding must not capture a free variable
  , testCase "let x = 1 in h x z" $
      case eval (C.Let (C.NonRec x (intLit 1)) (C.mkApps (C.Var hL2) [Left (C.Var x), Left (C.Var z)])) of
        r@(C.Letrec bs (C.App (C.App (C.Var hV) one) (C.Var zV)))
          | hV == hL2, zV == z -> do
              assertAlphaEq (intLit 1) one
              assertBool ("a binder captures z: " <> show r)
                (all ((/= C.varUniq z) . C.varUniq . fst) bs)
        r -> assertFailure ("unexpected result: " <> show r)

    -- FreeVarHeapBinding: a free variable that is an argument is let-bound,
    -- so that unwinding the stack finds it
  , testCase "an application to a free variable gets stuck" $
      assertAlphaEq (C.App (C.Var hL) (C.Var fL))
        (allocatedBody (eval (C.App (C.Var hL) (C.Var fL))))
  ]
 where
  fG = globalId User "f" 1000 intTy
  fL = intId User "f" 1000
  hL = C.mkLocalId (C.mkFunTy intTy intTy) (mkUnsafeSystemName "h" 1001)
  hL2 = C.mkLocalId (C.mkFunTy intTy (C.mkFunTy intTy intTy)) (mkUnsafeSystemName "h" 1002)

-- * Primitives

-- | A primitive with the given name and type
prim :: Text -> C.Type -> C.Term
prim nm ty = C.Prim (C.PrimInfo nm ty C.WorkNever C.SingleResult C.NoUnfolding)

integerLit :: Integer -> C.Term
integerLit = C.Literal . IntegerLiteral

nativeTests :: [TestTree]
nativeTests =
  [ testCase "integerAdd" $
      assertAlphaEq (integerLit 5)
        (eval (C.mkApps (prim "GHC.Num.Integer.integerAdd" integerBinTy)
                 [Left (integerLit 2), Left (integerLit 3)]))

  , testCase "integerAdd of a variable doesn't reduce" $
      let iX = C.mkLocalId integerPrimTy (mkUnsafeSystemName "i" 7)
          t = C.mkApps (prim "GHC.Num.Integer.integerAdd" integerBinTy)
                [Left (integerLit 2), Left (C.Var iX)]
      in assertAlphaEq t (eval t)

  , testCase "(&&) is lazy in its second argument" $
      -- False && b
      assertAlphaEq (C.Data ghcFalseDataCon)
        (allocatedBody (eval (C.mkApps andPrim [Left (C.Data ghcFalseDataCon), Left (C.Var b)])))

  , testCase "(&&) is lazy in its first argument" $
      -- b && False
      assertAlphaEq (C.Data ghcFalseDataCon)
        (allocatedBody (eval (C.mkApps andPrim [Left (C.Var b), Left (C.Data ghcFalseDataCon)])))

  , testCase "(||) of two constructors" $
      assertAlphaEq (C.Data ghcTrueDataCon)
        (allocatedBody (eval (C.mkApps orPrim
           [Left (C.Data ghcFalseDataCon), Left (C.Data ghcTrueDataCon)])))

    -- The arguments of (&&) are evaluated by nested machines, which allocate.
    -- The result continues in the last one, so the bindings it refers to are
    -- bound (#3266).
  , testCase "(&&) continues in the machine of its arguments" $
      -- True && (let w = b in w)
      let w = C.mkLocalId ghcBoolTy (mkUnsafeSystemName "w" 8)
          r = eval (C.mkApps andPrim
                [ Left (C.Data ghcTrueDataCon)
                , Left (C.Let (C.NonRec w (C.Var b)) (C.Var w)) ])
      in case r of
           C.Letrec bs _ ->
             assertBool "the result refers to a binding that isn't bound"
               (all (`elem` (b : map fst bs)) (freeIds r))
           _ -> assertFailure ("unexpected result: " <> show r)

  , testCase "head of map, as a case subject" $
      -- head (map (\x -> x) (1 :> 2 :> Nil))
      assertAlphaEq (intLit 1)
        (allocatedBody (evalSubj
          (C.mkApps (prim "Clash.Sized.Vector.head" headTy)
             [ Right (numTy 1), Right intTy
             , Left (C.mkApps (prim "Clash.Sized.Vector.map" mapTy)
                       [ Right intTy, Right intTy, Right (numTy 2)
                       , Left (C.Lam x (C.Var x)), Left (vec [intLit 1, intLit 2]) ]) ])))
  ]
 where
  integerBinTy = C.mkFunTy integerPrimTy (C.mkFunTy integerPrimTy integerPrimTy)
  boolBinTy = C.mkFunTy ghcBoolTy (C.mkFunTy ghcBoolTy ghcBoolTy)
  andPrim = prim "GHC.Classes.&&" boolBinTy
  orPrim = prim "GHC.Classes.||" boolBinTy
  b = C.mkLocalId ghcBoolTy (mkUnsafeSystemName "b" 6)
  freeIds = \case
    C.Letrec bs e -> filter (`notElem` map fst bs) (concatMap (freeIds . snd) bs ++ freeIds e)
    C.Var i -> [i]
    C.App e1 e2 -> freeIds e1 ++ freeIds e2
    C.TyApp e _ -> freeIds e
    C.Tick _ e -> freeIds e
    _ -> []
  tA = tyVar User "a" 9001
  tB = tyVar User "b" 9002
  tN = C.mkTyVar typeNatKind (mkUnsafeSystemName "n" 9003)
  v = C.VarTy
  -- head :: forall n a. Vec (n + 1) a -> a, with n + 1 written as a literal
  headTy = C.ForAllTy tN (C.ForAllTy tA (C.mkFunTy (vecTy (v tN) (v tA)) (v tA)))
  mapTy = C.ForAllTy tA (C.ForAllTy tB (C.ForAllTy tN
            (C.mkFunTy (C.mkFunTy (v tA) (v tB))
               (C.mkFunTy (vecTy (v tN) (v tA)) (vecTy (v tN) (v tB))))))
  vec xs = foldr (\(k, e) t -> cons k e t) nil (zip [fromIntegral (length xs), fromIntegral (length xs) - 1 ..] xs)
  nil = C.mkApps (C.Data nilDc) [Right (numTy 0), Right intTy, Left coercion]
  cons k e t = C.mkApps (C.Data consDc)
    [Right (numTy k), Right intTy, Right (numTy (k - 1)), Left coercion, Left e, Left t]
  coercion = C.Prim (C.PrimInfo "_CO_" intTy C.WorkNever C.SingleResult C.NoUnfolding)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator"
  [ testGroup "machine" machineTests
  , testGroup "tests/shouldwork/Evaluator" shouldworkTests
  , testGroup "ported primitives" nativeTests
  ]
