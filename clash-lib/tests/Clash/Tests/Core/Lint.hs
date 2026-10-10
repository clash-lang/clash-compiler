{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Lint"
-}

module Clash.Tests.Core.Lint (tests) where

import Data.Coerce (coerce)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Lint (LintError (..), newLintErrors)
import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Bind (..), CoreContext (..), Pat (..), Term (..))
import Clash.Core.Type (Type (..))
import Clash.Core.Var (Id, TyVar)
import Clash.Core.VarEnv (emptyInScopeSet)

import Test.Clash.Rewrite
  ( globalId, inScopeOf, intId, intLit, intTy, lintClosed, lintIn, localId
  , mkDataCon, pairDataCon, parseTyConTy, tyVar )

-- | Term variables of type @Int@
x, y :: Id
x = intId User "x" 1
y = intId User "y" 2

-- | A type variable, and a term variable of that type
a :: TyVar
a = tyVar User "a" 3

xa :: Id
xa = localId User "xa" 4 (VarTy a)

-- | A type variable and a term variable sharing a unique
a5 :: TyVar
a5 = tyVar User "a" 5

x5 :: Id
x5 = intId User "x" 5

-- | A global, and a local with the same unique
fGlobal, fLocal :: Id
fGlobal = globalId User "f" 6 intTy
fLocal = intId User "f" 6

-- | @data T = forall b. MkT b@, and a variable @s :: T@ to scrutinize
b :: TyVar
b = tyVar User "b" 7

yb :: Id
yb = localId User "yb" 8 (VarTy b)

mkT :: Pat
mkT = DataPat (mkDataCon "MkT" 200 [] [b] [VarTy b] (parseTyConTy "T")) [b] [yb]

s :: Id
s = localId User "s" 9 (parseTyConTy "T")

-- | The type variables of 'UnboundTyVar' errors
unboundTyVars :: [LintError] -> [TyVar]
unboundTyVars errs = [tv | UnboundTyVar tv _ <- errs]

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Lint"
  [ testGroup "unbound term variables"
      [ testCase "a closed term is well-scoped" $
          lintClosed (Lam x (Let (Rec [(y, Var x)]) (Var y))) @?= []
      , testCase "an unbound local is reported" $
          lintClosed (Lam x (Var y)) @?= [UnboundId y]
      , testCase "a local bound by a binder of the context is not reported" $
          lintIn [] emptyInScopeSet [LamBody y] (Lam x (Var y)) @?= []
      , testCase "a local in the in-scope set of the context is not reported" $
          lintIn [] (inScopeOf [y]) [] (Lam x (Var y)) @?= []
      , testCase "a non-recursive let does not bind its own right-hand side" $
          lintClosed (Let (NonRec x (Var x)) (Var x)) @?= [UnboundId x]
      , testCase "a type lambda does not bind a term variable with its unique" $
          lintClosed (TyLam a5 (Var x5)) @?= [UnboundId x5]
      , testCase "a type variable in the in-scope set does not bind a term variable" $
          lintIn [] (inScopeOf [a5]) [] (Var x5) @?= [UnboundId x5]
      , testCase "a global not in the binding map is reported" $
          lintClosed (Var fGlobal) @?= [UnknownGlobal fGlobal]
      , testCase "a global in the binding map is not reported" $
          lintIn [fGlobal] emptyInScopeSet [] (Var fGlobal) @?= []
      ]

  , testGroup "unbound type variables"
      [ testCase "in the type of a binder, and of its occurrence" $
          unboundTyVars (lintClosed (Lam xa (Var xa))) @?= [a, a]
      , testCase "reported once per type" $
          let xaa = localId User "xaa" 11 (AppTy (AppTy (parseTyConTy "Pair") (VarTy a)) (VarTy a))
          in unboundTyVars (lintClosed (Lam xaa (intLit 0))) @?= [a]
      , testCase "bound by a type lambda" $
          lintClosed (TyLam a (Lam xa (Var xa))) @?= []
      , testCase "bound by a type lambda in the context" $
          lintIn [] emptyInScopeSet [TyLamBody a] (Lam xa (Var xa)) @?= []
      , testCase "a term variable does not bind a type variable with its unique" $
          let xa3 = intId User "x" 3
          in unboundTyVars (lintClosed (Lam xa3 (Lam xa (Var xa)))) @?= [a, a]
      , testCase "in a type application" $
          unboundTyVars (lintIn [fGlobal] emptyInScopeSet [] (TyApp (Var fGlobal) (VarTy a)))
            @?= [a]
      , testCase "bound by a forall" $
          let v = localId User "v" 10 (ForAllTy a (VarTy a))
          in lintClosed (Lam v (Var v)) @?= []
      , testCase "an existential is bound by its pattern" $
          lintClosed (Lam s (Case (Var s) intTy [(mkT, Var yb)])) @?= []
      , testCase "an existential is not bound outside its alternative" $
          unboundTyVars (lintClosed (Lam s (Case (Var s) (VarTy b) [(mkT, Var yb)])))
            @?= [b]
      ]

  , testGroup "duplicate binders"
      [ testCase "in a let-expression in the right-hand side of a let-binding" $
          lintClosed
            (Let (Rec [(x, Let (Rec [(y, intLit 1), (y, intLit 2)]) (Var y))]) (Var x))
            @?= [DuplicateBinder (coerce y)]
      , testCase "in a pattern" $
          lintClosed
            (Lam s (Case (Var s) intTy [(DataPat pairDataCon [] [x, x], Var x)]))
            @?= [DuplicateBinder (coerce x)]
      ]

  , testGroup "shadowing"
      [ testCase "a lambda shadowing an enclosing lambda" $
          lintClosed (Lam x (Lam x (Var x))) @?= [Shadows (coerce x) (coerce x) False]
      , testCase "a let-binder shadowing an enclosing lambda" $
          lintClosed (Lam x (Let (Rec [(x, intLit 1)]) (Var x)))
            @?= [Shadows (coerce x) (coerce x) False]
      , testCase "a pattern binder shadowing an enclosing lambda" $
          lintClosed
            (Lam x (Lam s (Case (Var s) intTy [(DataPat pairDataCon [] [x, y], Var y)])))
            @?= [Shadows (coerce x) (coerce x) False]
      , testCase "a binder shadowing a binder of the context" $
          lintIn [] emptyInScopeSet [LamBody x] (Lam x (Var x))
            @?= [Shadows (coerce x) (coerce x) True]
      , testCase "a type lambda shadowing an enclosing type lambda" $
          lintClosed (TyLam a (TyLam a (intLit 0)))
            @?= [Shadows (coerce a) (coerce a) False]
      , testCase "binders in different branches may share a unique" $
          lintClosed (App (Lam x (Var x)) (Lam x (Var x))) @?= []
      , testCase "a type lambda does not shadow a term variable with its unique" $
          lintClosed (Lam x5 (TyLam a5 (Var x5))) @?= []
      ]

  , testGroup "local/global confusion"
      [ testCase "a local binder with the unique of a global" $
          lintIn [fGlobal] emptyInScopeSet [] (Lam fLocal (Var fLocal))
            @?= [LocalShadowsGlobal fLocal]
      ]

  , testGroup "newLintErrors"
      [ testCase "violations already in the input are not reported" $
          newLintErrors [UnboundId y] [UnboundId x, UnboundId y] @?= [UnboundId x]
      ]
  ]
