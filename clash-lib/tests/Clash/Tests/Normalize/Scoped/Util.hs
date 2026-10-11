{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Util": the regression tests of
  "Clash.Tests.Normalize.Util", and agreement with "Clash.Normalize.Util" on
  converted inputs.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Normalize.Scoped.Util (tests) where

import Control.Monad.State.Strict (evalState)
import Data.Default (def)
import qualified Data.List as List
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (..))
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Normalize.Types (NormalizeState (..))
import qualified Clash.Normalize.Util as Current
import Clash.Rewrite.Types (RewriteState (..))
import Clash.Unique (Unique)

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (mkTicks)
import Clash.Core.Scoped.TermInfo (isClosed)
import qualified Clash.Normalize.Scoped.Util as Scoped
import Clash.Rewrite.Scoped.Types (Frame (..), pushFrame, rootContext)

import Test.Clash.Rewrite
  ( globalId, intFunTy, intId, intLit, intTy, liftedEqTy, localId
  , mkBindingMap, nameToUnique, runRewriteTest, signalTy, systemDomTy, tyVar )
import Test.Clash.Scoped

-- * Type equality constraints

dom, dom2, a2, c3 :: C.TyVar
dom = tyVar User "dom" 1
a2 = tyVar User "a" 2
c3 = tyVar User "c" 3
dom2 = tyVar User "dom2" 8

-- | @a ~ b@
eqTy :: C.Type -> C.Type -> C.Type
eqTy = liftedEqTy

-- | Assert that 'Scoped.substWithTyEq' agrees with 'Current.substWithTyEq',
-- also in a scope where all binders of the term clash with the scope
agreesSubst :: C.Term -> Assertion
agreesSubst tm = withTermScopes tm $ \sc tm' ->
  assertScopedAlphaEq (Current.substWithTyEq tm) (Scoped.substWithTyEq sc tm')

-- | Assert the result of 'Scoped.substWithTyEq', also in a scope where all
-- binders of the term clash with the scope
substGives :: C.Term -> C.Term -> Assertion
substGives tm expected = withTermScopes tm $ \sc tm' ->
  assertScopedAlphaEq expected (Scoped.substWithTyEq sc tm')

-- | Assert the result of 'Scoped.tvSubstWithTyEq', also in a scope where all
-- binders of the type clash with the scope
tvSubstGives :: C.Type -> C.Type -> Assertion
tvSubstGives ty expected = withTypeScopes ty $ \sc ty' ->
  assertScopedTyAlphaEq expected (Scoped.tvSubstWithTyEq sc ty')

substWithTyEqTests :: [TestTree]
substWithTyEqTests =
  -- The regression tests of "Clash.Tests.Normalize.Util"
  [ testCase "substWithTyEq doesn't introduce free variables (#1060)" $ do
      -- /\dom -> \(eq :: dom ~ System) -> eq
      let eq = localId User "eq" 2 (eqTy (C.VarTy dom) systemDomTy)
          tm = C.TyLam dom (C.Lam eq (C.Var eq))
      agreesSubst tm
      -- \(eq :: System ~ System) -> removedArg @(System ~ System)
      assertBool "closed" (isClosed (Scoped.substWithTyEq emptyScope (fromClashTermClosed tm)))
  , testCase "substWithTyEq avoids capture (#1868)" $ do
      let eq = localId User "eq" 3 (eqTy (C.VarTy dom) (C.VarTy a2))
          x = localId User "x" 4 (C.VarTy dom)
      -- /\a -> /\dom -> \(eq :: dom ~ a) -> /\a -> \(x :: dom) -> x
      agreesSubst (C.TyLam a2 (C.TyLam dom (C.Lam eq (C.TyLam a2 (C.Lam x (C.Var x))))))
  , testCase "substWithTyEq keeps the order of TyLams and Lams (#1868)" $ do
      let eq = localId User "eq" 3 (eqTy (C.VarTy dom) systemDomTy)
          x = localId User "x" 4 (C.VarTy a2)
          y = localId User "y" 5 (C.VarTy a2)
      -- /\a -> \(x :: a) -> /\dom -> \(eq :: dom ~ System) -> \(y :: a) -> y
      agreesSubst (C.TyLam a2 (C.Lam x (C.TyLam dom (C.Lam eq (C.Lam y (C.Var y))))))

  , testCase "substWithTyEq handles several constraints, either way around" $ do
      let eq1 = localId User "eq1" 3 (eqTy (C.VarTy dom) systemDomTy)
          eq2 = localId User "eq2" 4 (eqTy systemDomTy (C.VarTy dom2))
          x = localId User "x" 5 (signalTy (C.VarTy dom2) intTy)
      -- /\dom -> /\dom2 -> \(eq1 :: dom ~ System) -> \(eq2 :: System ~ dom2) ->
      -- \(x :: Signal dom2 Int) -> eq1 `seq` x
      agreesSubst (C.TyLam dom (C.TyLam dom2 (C.Lam eq1 (C.Lam eq2 (C.Lam x
        (C.App (C.Var x) (C.Var eq1)))))))
  , testCase "substWithTyEq without constraints changes nothing" $ do
      let x = localId User "x" 4 (C.VarTy a2)
      agreesSubst (C.TyLam a2 (C.Lam x (C.Var x)))
      agreesSubst (intLit 3)

  , testCase "substWithTyEq moves a type lambda past binders that don't mention it" $ do
      let eq = localId User "eq" 3 (eqTy (C.VarTy dom) (C.VarTy a2))
          x = localId User "x" 4 (C.VarTy dom)
          y = localId User "y" 5 intTy
      -- /\dom -> \(y :: Int) -> /\a -> \(eq :: dom ~ a) -> \(x :: dom) -> x
      -- gives
      -- \(y :: Int) -> /\a -> \(eq :: a ~ a) -> \(x :: a) -> x
      let tm = C.TyLam dom (C.Lam y (C.TyLam a2 (C.Lam eq (C.Lam x (C.Var x)))))
      agreesSubst tm

    -- TODO(substWithTyEq): 'Current.substWithTyEq' leaves @dom@ free in the
    -- type of @c@. 'Scoped.substWithTyEq' can't: the type of @c@ is in scope
    -- of @dom@.
  , testCase "substWithTyEq substitutes in the binders before the constraint" $ do
      let c = localId User "c" 3 (signalTy (C.VarTy dom) intTy)
          eq = localId User "eq" 4 (eqTy (C.VarTy dom) systemDomTy)
          c' = localId User "c" 3 (signalTy systemDomTy intTy)
          eq' = localId User "eq" 4 (eqTy systemDomTy systemDomTy)
      -- /\dom -> \(c :: Signal dom Int) -> \(eq :: dom ~ System) -> c
      substGives (C.TyLam dom (C.Lam c (C.Lam eq (C.Var c))))
                 (C.Lam c' (C.Lam eq' (C.Var c')))
  , testCase "substWithTyEq keeps a constraint whose type isn't in scope" $ do
      let y = localId User "y" 3 (C.VarTy dom)
          eq = localId User "eq" 4 (eqTy (C.VarTy dom) (C.VarTy a2))
          -- /\dom -> \(y :: dom) -> /\a -> \(eq :: dom ~ a) -> y: the type of
          -- y can't mention a
          tm = C.TyLam dom (C.Lam y (C.TyLam a2 (C.Lam eq (C.Var y))))
      substGives tm tm
  ]

tvSubstWithTyEqTests :: [TestTree]
tvSubstWithTyEqTests =
  -- The regression test of "Clash.Tests.Normalize.Util"
  [ testCase "tvSubstWithTyEq avoids capture (#1868)" $
      -- forall a dom. (dom ~ a) -> forall a. dom -> a
      -- gives
      -- forall a. (a ~ a) -> forall c. a -> c
      tvSubstGives
        (C.ForAllTy a2 $ C.ForAllTy dom $
          eqTy (C.VarTy dom) (C.VarTy a2) `C.mkFunTy`
            C.ForAllTy a2 (C.VarTy dom `C.mkFunTy` C.VarTy a2))
        (C.ForAllTy a2 $
          eqTy (C.VarTy a2) (C.VarTy a2) `C.mkFunTy`
            C.ForAllTy c3 (C.VarTy a2 `C.mkFunTy` C.VarTy c3))

    -- TODO(substWithTyEq): 'Current.tvSubstWithTyEq' leaves @dom@ free in the
    -- constraint
  , testCase "tvSubstWithTyEq substitutes in the constraint" $
      -- forall dom. (dom ~ System) -> Signal dom Int
      tvSubstGives
        (C.ForAllTy dom (eqTy (C.VarTy dom) systemDomTy `C.mkFunTy` signalTy (C.VarTy dom) intTy))
        (eqTy systemDomTy systemDomTy `C.mkFunTy` signalTy systemDomTy intTy)
  , testCase "tvSubstWithTyEq substitutes in the arguments before the constraint" $
      -- forall dom. Signal dom Int -> (System ~ dom) -> Int
      tvSubstGives
        (C.ForAllTy dom (signalTy (C.VarTy dom) intTy `C.mkFunTy`
          (eqTy systemDomTy (C.VarTy dom) `C.mkFunTy` intTy)))
        (signalTy systemDomTy intTy `C.mkFunTy`
          (eqTy systemDomTy systemDomTy `C.mkFunTy` intTy))
  , testCase "tvSubstWithTyEq moves a forall past binders that don't mention it" $
      -- forall dom a. (dom ~ a) -> dom
      tvSubstGives
        (C.ForAllTy dom $ C.ForAllTy a2 $ eqTy (C.VarTy dom) (C.VarTy a2) `C.mkFunTy` C.VarTy dom)
        (C.ForAllTy a2 $ eqTy (C.VarTy a2) (C.VarTy a2) `C.mkFunTy` C.VarTy a2)
  , testCase "tvSubstWithTyEq without constraints changes nothing" $ do
      let ty = C.ForAllTy a2 (C.VarTy a2 `C.mkFunTy` C.VarTy a2)
      assertAgreesTy Current.tvSubstWithTyEq Scoped.tvSubstWithTyEq ty
      tvSubstGives ty ty
  ]

-- * Recursive globals

gGlobal, gLocal, xRec :: C.Id
gGlobal = globalId User "g" 7 (intFunTy 1)
gLocal = localId User "g" 7 (intFunTy 1)
xRec = localId User "x" 8 (intFunTy 0)

-- | @g = \\x -> g x@
recState :: RewriteState NormalizeState
recState = def
  { _bindings = mkBindingMap [(gGlobal, C.Lam xRec (C.App (C.Var gGlobal) (C.Var xRec)))] }

recursionTests :: [TestTree]
recursionTests =
  [ testCase "isNonRecursiveGlobalVar doesn't confuse a local with a global" $ do
      -- The local variable keeps the unique of the global
      ((localRes, globalRes, recRes), _, _) <- runRewriteTest def recState $
        withConvEnv [] [gLocal] $ \env -> do
          l <- Scoped.isNonRecursiveGlobalVar (fromClashTerm env (C.Var gLocal))
          g <- Scoped.isNonRecursiveGlobalVar (GlobalVar (mkGlobalId gGlobal) :: Term 'VoidS)
          r <- Scoped.isRecursiveBndr (mkGlobalId gGlobal)
          pure (l, g, r)
      assertBool "local variable is a non-recursive global" (not localRes)
      assertBool "recursive global is non-recursive" (not globalRes)
      assertBool "recursive global is non-recursive" recRes
  , testCase "isRecursiveBndr of a global without a binding" $ do
      (r, _, _) <- runRewriteTest def (def :: RewriteState NormalizeState) $
        Scoped.isRecursiveBndr (mkGlobalId gGlobal)
      r @?= False
  ]

-- * Inlining

inliningTests :: [TestTree]
inliningTests =
  [ testCase "addNewInline counts inlinings per function" $ do
      let f = mkGlobalId (globalId User "f" 10 intTy)
          cf = mkGlobalId (globalId User "cf" 11 intTy)
          res = flip evalState def $ do
            Scoped.addNewInline f cf
            Scoped.addNewInline f cf
            (,) <$> Scoped.alreadyInlined f cf <*> Scoped.alreadyInlined cf f
      res @?= (Just 2, Nothing)
  , testCase "mkInlineTick agrees" $ do
      let f = globalId User "Some.Module.f" 10 intTy
      assertScopedAlphaEq
        (C.mkTicks (intLit 1) [Current.mkInlineTick f])
        (mkTicks (Literal (IntLiteral 1)) [Scoped.mkInlineTick (mkGlobalId f)] :: Term 'VoidS)
  ]

-- * Constant arguments

shouldReduceTests :: [TestTree]
shouldReduceTests =
  [ testCase "shouldReduce in an argument a primitive wants constant" $ do
      (r, _, _) <- runRewriteTest def (def :: RewriteState NormalizeState) $
        Scoped.shouldReduce (argCtx "Clash.Explicit.SimIO.mealyIO" 2)
      r @?= True
  , testCase "shouldReduce elsewhere" $ do
      (r, _, _) <- runRewriteTest def (def :: RewriteState NormalizeState) $
        (,) <$> Scoped.shouldReduce (argCtx "unknownPrim" 2)
            <*> Scoped.shouldReduce (rootContext emptyScope)
      r @?= (False, False)
  ]
 where
  argCtx nm i = pushFrame (AppArg (Just (nm, 0, i))) (rootContext emptyScope)

-- * Call graphs

callGraphTests :: [TestTree]
callGraphTests =
  [ testCase "callGraph agrees" $ do
      let cur = Current.callGraph (mkBindingMap bs) f
          sco = Scoped.callGraph (mkScopedBindingMap bs) (mkGlobalId f)
      flatten cur @?= flattenS sco
      List.sort (foldr (:) [] (Current.collectCallGraphUniques cur))
        @?= List.sort (map globalIdUniq (globalSetToList (Scoped.callGraphGlobals sco)))
  ]
 where
  fn nm = globalId User nm (nameToUnique nm) (intFunTy 1)
  f = fn "f"
  g = fn "g"
  h = fn "h"
  k = fn "k"
  x = intId User "x" 1
  -- f = \x -> g (g x); g = \x -> h x; h = \x -> x; k = \x -> f x
  bs =
    [ (f, C.Lam x (C.App (C.Var g) (C.App (C.Var g) (C.Var x))))
    , (g, C.Lam x (C.App (C.Var h) (C.Var x)))
    , (h, C.Lam x (C.Var x))
    , (k, C.Lam x (C.App (C.Var f) (C.Var x)))
    ]

  flatten :: UniqMap.UniqMap (UniqMap.UniqMap Word) -> [(Unique, [(Unique, Word)])]
  flatten cg = List.sort
    [ (u, List.sort (UniqMap.toList m)) | (u, m) <- UniqMap.toList cg ]

  flattenS :: Scoped.CallGraph -> [(Unique, [(Unique, Word)])]
  flattenS cg = List.sort
    [ (globalIdUniq u, List.sort [(globalIdUniq v, n) | (v, n) <- globalEnvToList m])
    | (u, m) <- globalEnvToList cg ]

-- * Classifying functions

classifyTests :: [TestTree]
classifyTests =
  [ testCase ("classifyFunction agrees on " <> show i) $ withFreeVarsOf [tm] $ \env -> do
      let s = fromClashTerm env tm
      show (Scoped.classifyFunction s) @?= show (Current.classifyFunction tm)
      Scoped.isCheapFunction s @?= Current.isCheapFunction tm
  | (i, tm) <- zip [0 :: Int ..] terms
  ]
 where
  f = globalId User "f" 20 (intFunTy 1)
  p = C.Prim somePrim
  x = intId User "x" 1
  y = intId User "y" 2
  terms =
    [ C.Lam x (C.App (C.Var f) (C.Var x))
    , C.Lam x (C.App (C.Var y) (C.Var x))
    , C.Lam x (C.Let (C.NonRec y (C.App (C.Var f) (C.Var x))) (C.App (C.Var f) (C.Var y)))
    , C.Lam x (C.Case (C.Var x) intTy
        [ (C.LitPat (IntLiteral 1), C.App (C.Var f) (C.Var x))
        , (C.DefaultPat, C.App p (C.Var x)) ])
    , C.Let (C.Rec [(y, C.App p (C.Var y))]) (C.App p (C.App (C.Var f) (C.Var y)))
    , C.Tick C.DeDup (C.App p (C.Var x))
    ]
tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Scoped.Util"
    [ testGroup "substWithTyEq" substWithTyEqTests
    , testGroup "tvSubstWithTyEq" tvSubstWithTyEqTests
    , testGroup "recursion" recursionTests
    , testGroup "inlining" inliningTests
    , testGroup "shouldReduce" shouldReduceTests
    , testGroup "callGraph" callGraphTests
    , testGroup "classifyFunction" classifyTests
    ]
