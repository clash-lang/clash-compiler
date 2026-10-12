{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator": the whnf evaluator of scoped Clash
  Core, its ported primitive reductions, and the adapter that reduces the
  other primitives with the evaluator of Clash Core.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Evaluator (tests) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Evaluator.Types as CE
import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (..), mkUnsafeSystemName)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Core.TysPrim (intPrimTy, integerPrimTy, typeNatKind)
import qualified Clash.Core.Var as C
import Clash.Core.VarEnv (extendInScopeSet, uniqAway)
import Clash.Driver.Types (BindingMap)
import Clash.Util.Supply (freshId)

import Test.Clash.Scoped.Evaluator.Adapter (adaptPrim)
import Clash.Core.Scoped.Evaluator.Primitives (scopedPrimReductions)
import Clash.Core.Scoped.Evaluator.Types (Env (..), PrimReductions (..))

import Test.Clash.Rewrite
  ( assertAlphaEq, consDc, ghcBoolTy, ghcFalseDataCon, ghcTrueDataCon
  , globalId, heapLookupEvaluator, intId, intLit, intTy, justDataCon
  , maybeTy, mkBindingMap, nilDc, numTy, pairDataCon, pairTy, tyVar
  , vecTy )
import Test.Clash.Scoped (scopedWhnf, testTypeEnv)

-- | Evaluate with the test types, no globals, and no primitives of Clash Core
eval :: C.Term -> C.Term
eval = evalWith heapLookupEvaluator mempty False

-- | Evaluate as the subject of a case-expression
evalSubj :: C.Term -> C.Term
evalSubj = evalWith heapLookupEvaluator mempty True

-- | Evaluate with the ported primitive reductions, and the given evaluator of
-- Clash Core for the other primitives
evalWith :: CE.Evaluator -> BindingMap -> Bool -> C.Term -> C.Term
evalWith clashEval = scopedWhnf
  (Env testTypeEnv (PrimReductions scopedPrimReductions (Just (adaptPrim clashEval))))

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
        (evalWith heapLookupEvaluator (mkBindingMap [(g, intLit 42)]) False (C.Var g))

    -- A local and a global with the same unique (decision D5 of the port)
  , testCase "locals are not looked up in the global heap" $
      assertAlphaEq (C.Var xG)
        (evalWith heapLookupEvaluator (mkBindingMap [(g, intLit 42)]) False (C.Var xG))

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
        (allocatedBody (evalWith heapLookupEvaluator (mkBindingMap [(fG, intLit 1)]) False
          (C.Let (C.NonRec fL (intLit 2)) (C.Var fG))))

  , testCase "a global argument is not the local with its unique" $
      -- let fLocal = 2 in h f
      assertAlphaEq (C.App (C.Var hL) (C.Var fG))
        (allocatedBody (evalWith heapLookupEvaluator (mkBindingMap [(fG, intLit 1)]) False
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

-- * The adapter

-- | An evaluator of Clash Core with primitive reductions for the adapter:
--
-- * @plus@ adds two @Int@ literals;
-- * @alloc@ allocates its argument on the heap, and applies @f@ to it;
-- * @fstField@ evaluates the first field of a pair, with the machine of
--   Clash Core.
adapterEvaluator :: CE.Evaluator
adapterEvaluator = heapLookupEvaluator
  { CE.primStep = \tcm _isSubj p _tys args m -> case (C.primName p, args) of
      ("plus", [CE.Lit (IntLiteral i), CE.Lit (IntLiteral j)]) ->
        Just (CE.setTerm (intLit (i + j)) m)
      ("alloc", [a]) ->
        let (u, sup) = freshId (CE.mSupply m)
            hv = uniqAway (CE.mScopeNames m) (intId System "h" u)
            m1 = m {CE.mSupply = sup, CE.mScopeNames = extendInScopeSet (CE.mScopeNames m) hv}
        in Just (CE.setTerm (C.App (C.Var fG) (C.Var hv))
                   (CE.heapInsert C.LocalId hv (CE.valToTerm a) m1))
      ("fstField", [CE.DC _ (Left a : _)]) ->
        let m1 = CE.whnf heapLookupEvaluator tcm False (CE.setTerm a (CE.stackClear m))
        in Just m1
      _ -> Nothing
  }
 where
  fG = globalId User "f" 5 (C.mkFunTy intTy intTy)

adapterTests :: [TestTree]
adapterTests =
  [ testCase "a primitive that inspects literals" $
      assertAlphaEq (intLit 5)
        (evalWith adapterEvaluator mempty False
          (C.mkApps (prim "plus" intBinTy) [Left (intLit 2), Left (intLit 3)]))

  , testCase "bindings the reduction allocates become allocated bindings" $
      -- alloc (MkPair x x): its argument is bound by a new binding, which f
      -- is applied to
      case evalWith adapterEvaluator mempty False
             (C.App (prim "alloc" (C.mkFunTy pairTy intTy))
                    (C.mkApps (C.Data pairDataCon) [Left (C.Var x), Left (C.Var x)])) of
        -- Unwinding the stack replaces the argument of f by its binding, like
        -- the machine of Clash Core does
        r@(C.Letrec [(h, rhs)] (C.App (C.Var fV) arg))
          | C.varUniq fV == 5
          -> do
            assertAlphaEq (C.mkApps (C.Data pairDataCon) [Left (C.Var x), Left (C.Var x)]) rhs
            assertAlphaEq rhs arg
            assertBool ("allocated binder clashes with x: " <> show r) (C.varUniq h /= C.varUniq x)
        r -> assertFailure ("unexpected result: " <> show r)

  , testCase "a nested evaluation sees the heap bindings the arguments reach" $
      -- let y = 7 in fstField (MkPair y y): the machine of Clash Core gets
      -- the binding of y
      assertAlphaEq (intLit 7)
        (allocatedBody (evalWith adapterEvaluator mempty False
          (C.Let (C.NonRec y (intLit 7))
            (C.App (prim "fstField" (C.mkFunTy pairTy intTy))
                   (C.mkApps (C.Data pairDataCon) [Left (C.Var y), Left (C.Var y)])))))
  ]
 where
  intBinTy = C.mkFunTy intTy (C.mkFunTy intTy intTy)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator"
  [ testGroup "machine" machineTests
  , testGroup "tests/shouldwork/Evaluator" shouldworkTests
  , testGroup "ported primitives" nativeTests
  , testGroup "adapter" adapterTests
  ]
