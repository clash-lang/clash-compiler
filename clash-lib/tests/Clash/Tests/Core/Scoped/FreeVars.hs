{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the free variables of scoped Clash Core. Ports of the tests in
  "Clash.Tests.Core.FreeVars", and tests of occurrence checks.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.Core.Scoped.FreeVars (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.FreeVars
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite
  ( globalId, intId, intTy, kindedTyVar, localId, pairDataCon, pairTy
  , parseToTermQQ, tyVar)
import Test.Clash.Scoped (extendSunk, nameSetRaws, withFreeVarsOf)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.FreeVars"
  [ -- @\f -> g f@, where the local @f@ and the global @g@ have the same unique
    testCase "local and global variables are kept apart" $ do
      let fL = intId C.User "f" 20
          fG = globalId C.User "f" 20 intTy
          tm = fromClashTermClosed (C.Lam fL (C.App (C.Var fG) (C.Var fL)))
      globalSetToList (termGlobalIds tm) @?= [mkGlobalId fG]
      assertBool "no free local variables" (nullNameSet (freeTmVars tm))

    -- In @forall k. b@ with @b :: k@, the @k@ in the kind of the free @b@ is
    -- not the @k@ bound by the @forall@, so it's free.
    -- https://github.com/clash-lang/clash-compiler/issues/398,
    -- https://github.com/clash-lang/clash-compiler/pull/446
  , testCase "free type variables close over kinds (#446)" $ do
      let k = tyVar C.User "k" 1
          b = kindedTyVar C.User "b" 2 (C.VarTy k)
          ty = C.ForAllTy k (C.VarTy b)
      withConvEnv [k, b] [] $ \env ->
        nameSetRaws (freeTyVarsOfType (fromClashType env ty)) @?= [1, 2]

    -- The term level equivalent: in @/\k -> a@ with @a :: k@, the @k@ in the
    -- type of the free @a@ is free.
  , testCase "free type variables of a term close over types (#446)" $ do
      let k = tyVar C.User "k" 1
          a = localId C.User "a" 2 (C.VarTy k)
          tm = C.TyLam k (C.Var a)
      withConvEnv [k] [a] $ \env -> do
        let tm1 = fromClashTerm env tm
        nameSetRaws (freeTyVars tm1) @?= [1]
        nameSetRaws (freeTmVars tm1) @?= [2]

    -- In Clash Core, the binder of a type lambda hides a free term variable
    -- with the same unique, as both live in one in-scope set. In scoped Core,
    -- the binder is renamed, and the term variable stays free.
  , testCase "a type variable binder doesn't hide a term variable" $ do
      let a = tyVar C.User "a" 20
          x = intId C.User "x" 20
          tm = C.TyLam a (C.Var x)
      withConvEnv [] [x] $ \env -> do
        let tm1 = fromClashTerm env tm
        nameSetRaws (freeTmVars tm1) @?= [20]
        assertBool "no free type variables" (nullNameSet (freeTyVars tm1))

  , testCase "freeVars splits type and term variables" $ do
      let tm = [parseToTermQQ| \(y :: Int) -> f_10 y (x_11 :: a_12) |]
      withFreeVarsOf [tm] $ \env -> do
        let (tvs, ids) = freeVars (fromClashTerm env tm)
        nameSetRaws tvs @?= [12]
        nameSetRaws ids @?= [10, 11]

  , testCase "occurrence checks look at the types of occurrences" $ do
      -- The type variable only occurs in the type of the free variable x
      let a = tyVar C.User "a" 1
          x = localId C.User "x" 2 (C.VarTy a)
      withConvEnv [a] [x] $ \env -> case fromClashTerm env (C.Var x) of
        tm@(Var _ (VarTy _ _ an) xn) -> do
          assertBool "a occurs" (tyNameOccursIn an tm)
          assertBool "x occurs" (tmNameOccursIn xn tm)
        _ -> assertFailure "unexpected shape"

  , testCase "occurrence checks account for shadowing binders" $ do
      -- Convert @\y -> y@ in the empty scope, and sink it into the scope of
      -- 'sunkVar' and @x@, where its binder has the same name as @x@
      let lam = fromClashTermClosed [parseToTermQQ| \(y_1 :: Int) -> y_1 |]
          x = intId C.User "x" 1
      extendSunk emptyConvEnv [] [x] $ \env -> case fromClashTerm env (C.Var x) of
        Var _ _ xn -> do
          let tm = sink lam
          assertBool "x does not occur" (not (tmNameOccursIn xn tm))
          assertBool "no free variables" (nullNameSet (freeTmVars tm))
        _ -> assertFailure "unexpected shape"

  , testCase "patBindersOccurIn" $ do
      -- @\s -> case s of MkPair a b -> a@
      let s = localId C.User "s" 1 pairTy
          a = intId C.User "a" 2
          b = intId C.User "b" 3
          tm = C.Lam s (C.Case (C.Var s) intTy [(C.DataPat pairDataCon [] [a, b], C.Var a)])
      case fromClashTermClosed tm of
        Lam _ (Case _ _ [Alt p e]) -> do
          assertBool "a occurs" (patBindersOccurIn p e)
          case p of
            DataPat _ TNil (TCons ba (TCons _ TNil)) ->
              assertBool "only a occurs"
                (nameSetRaws (freeTmVars e) == [tmNameRaw (tmNameOf ba)])
            _ -> assertFailure "unexpected pattern"
        _ -> assertFailure "unexpected shape"
  ]
