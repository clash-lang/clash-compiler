{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Evaluator": the whnf evaluator of Clash Core and its primitive reductions, in the environment Clash uses
  ('Clash.Rewrite.Whnf.evaluatorEnv').
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Core.Evaluator (tests) where

import qualified Data.IntSet as IntSet
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Driver.Types (Binding)

import Clash.Core.Alpha (aeqTm)
import Clash.Core.Builder (dataB)
import Clash.Core.Evaluator.Types (Env)
import Clash.Core.NameMap (GlobalEnv, emptyGlobalEnv)
import Clash.Core.Subst (closeTerm)
import Clash.Core.Syntax
import Clash.Rewrite.Whnf (evaluatorEnv)

import Test.Clash.Rewrite (ghcFalseDataCon, ghcTrueDataCon, showPprU)
import Test.Clash.Core
  ( TestScope (..), TestTerm, assertLintClean, assertTermAlphaEq, assertTermIs, atRaw
  , builtTerm, closedTerm, mkGlobalBindings, termIn, testGlobalId, testScopeRaws, testTypeEnv, varD
  , withTerm, withTestScope )
import Test.Clash.Core.Evaluator (allocatedBody, assertSameValue, nfDataIn, whnfIn)
import Test.Clash.Core.Evaluator.Primitives

-- | The environment of Clash, with the types of the tests
env :: Env
env = evaluatorEnv testTypeEnv

-- | Evaluate a closed term, with no globals
evalT :: TestTerm -> Term 'VoidS
evalT = evalTWith emptyGlobalEnv

-- | Evaluate a closed term with the given globals
evalTWith :: GlobalEnv (Binding (Term 'VoidS)) -> TestTerm -> Term 'VoidS
evalTWith gs t = whnfIn env gs False emptyScope (closedTerm t)

-- | Evaluate a term in a test scope with the given globals
evalIn :: Distinct n => GlobalEnv (Binding (Term 'VoidS)) -> TestScope n -> TestTerm -> Term n
evalIn gs ts t = whnfIn env gs False (testScope ts) (termIn ts t)

-- | The raw names of the bindings the evaluator allocated around a result
allocatedRaws :: Term n -> [Int]
allocatedRaws = \case
  Let (NonRec b _) _ -> [tmNameRaw (tmNameOf b)]
  Let (Rec bs _) _ -> teleRaws bs
  _ -> []
 where
  teleRaws :: Tele IdBinder x y -> [Int]
  teleRaws = \case
    TNil -> []
    TCons b bs -> tmNameRaw (tmNameOf b) : teleRaws bs

machineTests :: [TestTree]
machineTests =
  [ testCase "beta reduction allocates the argument" $
      -- The binding has the type of the literal
      assertTermAlphaEq (closedTerm "let { (h :: GHC.Prim.Int#) = 3 } in 3")
        (evalT "(\\(x :: Int) -> x) 3")

  , testCase "let-bindings are allocated" $
      assertTermAlphaEq (closedTerm "let { (x :: Int) = 1 } in 1")
        (evalT "let { !(x :: Int) = 1 } in x")

  , testCase "a self-referencing binding doesn't loop" $
      case evalT "let { (x :: Int) = x } in x" of
        Let (Rec (TCons _ TNil) _) _ -> pure ()
        r -> assertFailure ("unexpected result: " <> showPprU r)

  , testCase "case of a known constructor substitutes its fields" $
      assertTermAlphaEq (closedTerm "2")
        (evalT "(case MkPair 1 2 of { MkPair (y :: Int) (z :: Int) -> z }) :: Int")

  , testCase "case of a literal selects its alternative" $
      assertTermAlphaEq (closedTerm "20")
        (evalT "(case 2 of { _ -> 30; 1 -> 10; 2 -> 20 }) :: Int")

  , testCase "a free variable is a value" $
      withTerm "x_1 :: Int" $ \ts e ->
        assertTermIs ts "x_1" (whnfIn env emptyGlobalEnv False (testScope ts) e)

  , testCase "globals are looked up in the global heap" $
      assertTermAlphaEq (closedTerm "42") (evalTWith globals "g_G4 :: Int")

    -- A local and a global with the same name: the raw name of the local is
    -- the unique of the global
  , testCase "locals are not looked up in the global heap" $
      withTestScope [atRaw 4 (varD "x_4" "Int")] $ \ts ->
        assertTermIs ts "x_4" (evalIn globals ts "x_4")

    -- The machine doesn't expand a partial application at the top: there is
    -- nothing to apply the abstraction to.
  , testCase "a partially applied constructor at the top is not eta-expanded" $
      assertTermAlphaEq (closedTerm "Just @Int") (evalT "Just @Int")

  , testCase "partially applied constructors are eta-expanded" $
      let r = evalT "(\\(f :: Int -> Maybe Int) -> f 1) (Just @Int)"
          expanded = closedTerm "\\(x :: Int) -> Just @Int x"
      in do
        assertSameValue emptyScope (closedTerm "Just @Int 1") r
        case r of
          Let (Rec _ rhss) _ ->
            assertBool ("the expansion is not bound: " <> showPprU r)
              (any (maybe False (aeqTm expanded) . closeTerm) rhss)
          _ -> assertFailure ("unexpected result: " <> showPprU r)

    -- (\y -> MkPair y x) (f 1), where the argument is allocated. Its binder
    -- must not be x, which has the highest raw name of the scope: the name
    -- that a machine that doesn't rename the binder above the scope would
    -- give it.
  , testCase "a binding the machine allocates is fresh for the free variables" $
      withTestScope [varD "x_1" "Int"] $ \ts -> do
        let r = evalIn emptyGlobalEnv ts "(\\(y :: Int) -> MkPair y x_1) ((f_G5 :: Int -> Int) 1)"
        assertLintClean ts r
        assertTermIs ts "let { (h :: Int) = (f_G5 :: Int -> Int) 1 } in MkPair h x_1" r
        assertBool "allocated binder clashes with a free variable"
          (not (any (`IntSet.member` testScopeRaws ts) (allocatedRaws r)))
  ]
 where
  globals = mkGlobalBindings [(testGlobalId "g_G4" "Int", "42")]

-- | The tests of @tests/shouldwork/Evaluator@ that test the machine
shouldworkTests :: [TestTree]
shouldworkTests =
  -- GlobalLocalHeapLookup: a global and a local share a name: the local
  -- binder gets raw name 1000, the unique of the global, as the watermark of
  -- the scope is 999
  [ testCase "a global is not looked up in the local heap" $
      withTestScope [atRaw 999 (varD "w_1" "Int")] $ \ts -> do
        let e = termIn ts "let { !(f_1000 :: Int) = 2 } in (f_G1000 :: Int)"
        case e of
          Let (NonRec b _) _ -> tmNameRaw (tmNameOf b) @?= 1000
          _ -> assertFailure ("unexpected input: " <> showPprU e)
        assertTermIs ts "1"
          (allocatedBody (whnfIn env fGlobal False (testScope ts) e))

  , testCase "a global argument is not the local with its name" $
      withTestScope [atRaw 999 (varD "h_1001" "Int -> Int")] $ \ts ->
        assertTermIs ts "h_1001 (f_G1000 :: Int)"
          (allocatedBody (evalIn fGlobal ts "let { !(f_1000 :: Int) = 2 } in h_1001 (f_G1000 :: Int)"))

    -- LetAllocationFreshness: a let-binding must not capture a free
    -- variable. z has the highest raw name of the scope. The arguments of h
    -- are allocated too (see FreeVarHeapBinding), and unwinding the stack
    -- puts their values back.
  , testCase "let x = 1 in h x z" $
      withTestScope [varD "h_1002" "Int -> Int -> Int", varD "z_3" "Int"] $ \ts -> do
        let r = evalIn emptyGlobalEnv ts "let { !(x :: Int) = 1 } in h_1002 x z_3"
        assertLintClean ts r
        assertTermIs ts "let { (x :: Int) = 1; (y :: Int) = z_3 } in h_1002 1 z_3" r
        assertBool ("a binder captures z: " <> showPprU r) (2 `notElem` allocatedRaws r)

    -- FreeVarHeapBinding: a free variable that is an argument is let-bound,
    -- so that unwinding the stack finds it
  , testCase "an application to a free variable gets stuck" $
      withTestScope [varD "h_1001" "Int -> Int", varD "f_1000" "Int"] $ \ts ->
        assertTermIs ts "h_1001 f_1000" (allocatedBody (evalIn emptyGlobalEnv ts "h_1001 f_1000"))
  ]
 where
  fGlobal = mkGlobalBindings [(testGlobalId "f_G1000" "Int", "1")]

-- * Primitives

nativeTests :: [TestTree]
nativeTests =
  [ testCase "integerAdd" $
      assertEvalsTo (integerLit 5)
        (apps (prim "GHC.Num.Integer.integerAdd" integerBinTy) [Left (integerLit 2), Left (integerLit 3)])

  , testCase "integerAdd of a variable doesn't reduce" $
      let t = apps (prim "GHC.Num.Integer.integerAdd" integerBinTy) [Left (integerLit 2), Left (var "i_7")]
      in evalOpen [("i_7", integerPrimTy)] False t $ \ts r -> assertTermIs ts (builtTerm t) r

  , testCase "(&&) is lazy in its second argument" $
      evalOpen [("b_6", ghcBoolTy)] False (apps andPrim [Left (dataB ghcFalseDataCon), Left (var "b_6")]) $ \ts r ->
        assertTermIs ts (builtTerm (dataB ghcFalseDataCon)) (allocatedBody r)

  , testCase "(&&) is lazy in its first argument" $
      evalOpen [("b_6", ghcBoolTy)] False (apps andPrim [Left (var "b_6"), Left (dataB ghcFalseDataCon)]) $ \ts r ->
        assertTermIs ts (builtTerm (dataB ghcFalseDataCon)) (allocatedBody r)

  , testCase "(||) of two constructors" $
      assertTermAlphaEq (closed (dataB ghcTrueDataCon))
        (allocatedBody (eval (apps orPrim [Left (dataB ghcFalseDataCon), Left (dataB ghcTrueDataCon)])))

    -- The arguments of (&&) are evaluated by nested machines, which allocate.
    -- The result continues in the last one, so the bindings it refers to are
    -- bound (#3266).
  , testCase "(&&) continues in the machine of its arguments" $
      -- True && (let w = b in w)
      evalOpen [("b_6", ghcBoolTy)] False
        (apps andPrim [Left (dataB ghcTrueDataCon), Left (letNonRec "w_8" ghcBoolTy (var "b_6") id)]) $
          \ts r -> assertLintClean ts r

  , testCase "head of map, as a case subject" $
      -- head (map (\x -> x) (1 :> 2 :> Nil))
      assertTermAlphaEq (closed (intLit 1))
        (allocatedBody (evalSubj
          (apps (prim "Clash.Sized.Vector.head" headTy)
             [ Right (numTy 1), Right intTy
             , Left (apps (prim "Clash.Sized.Vector.map" mapTy)
                       [ Right intTy, Right intTy, Right (numTy 2)
                       , Left (lam "x_1" intTy id), Left (vec intTy [intLit 1, intLit 2]) ]) ])))
  ]
 where
  intTy = ty "Int"
  integerBinTy = binTy integerPrimTy integerPrimTy
  boolBinTy = binTy ghcBoolTy ghcBoolTy
  andPrim = prim "GHC.Classes.&&" boolBinTy
  orPrim = prim "GHC.Classes.||" boolBinTy
  -- head :: forall n a. Vec (n + 1) a -> a, with n + 1 written as a literal
  headTy = forAll "n_9003" natKind $ \n -> forAll "a_9001" typeKind $ \a -> vecTy n a ~> a
  mapTy = forAll "a_9001" typeKind $ \a -> forAll "b_9002" typeKind $ \b ->
    forAll "n_9003" natKind $ \n -> (a ~> b) ~> vecTy n a ~> vecTy n b

-- | 'Clash.Core.Evaluator.nfData'
nfDataTests :: [TestTree]
nfDataTests =
  [ testCase "the fields of a constructor are evaluated" $
      assertTermAlphaEq (closedTerm "MkPair 1 2")
        (allocatedBody (nf "MkPair ((\\(x :: Int) -> x) 1) (let { !(y :: Int) = 2 } in y)"))

  , testCase "nested constructors are evaluated" $
      assertTermAlphaEq (closedTerm "Just @Pair (MkPair 1 2)")
        (allocatedBody (nf "Just @Pair (MkPair 1 ((\\(x :: Int) -> x) 2))"))

  , testCase "a term that isn't a constructor application is evaluated to WHNF" $
      assertTermAlphaEq (closedTerm "3") (allocatedBody (nf "(\\(x :: Int) -> x) 3"))
  ]
 where
  nf t = nfDataIn env emptyGlobalEnv emptyScope (closedTerm t)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Evaluator"
  [ testGroup "machine" machineTests
  , testGroup "nfData" nfDataTests
  , testGroup "tests/shouldwork/Evaluator" shouldworkTests
  , testGroup "ported primitives" nativeTests
  ]
