{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for alpha-equivalence of scoped Clash Core. Includes the examples of
  "Clash.Tests.Core.AlphaEquivalence".
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.Core.Scoped.Alpha (tests) where

import Data.Maybe (isNothing)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C

import Clash.Core.Scoped.Alpha
import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite
  (intId, intTy, localId, mkDataCon, parseToTermQQ, parseTyConTy, tyVar)
import Test.Clash.Scoped (assertScopedAlphaEq, extendSunk, withFreeVarsOf)

-- | Whether two Clash terms are alpha-equivalent as scoped terms, converted
-- in the scope of their free variables
aeqConverted :: C.Term -> C.Term -> Bool
aeqConverted t1 t2 =
  withFreeVarsOf [t1, t2] $ \env -> aeqTm (fromClashTerm env t1) (fromClashTerm env t2)

assertAeq :: C.Term -> C.Term -> Assertion
assertAeq t1 t2 = do
  assertBool "t1 == t2" (aeqConverted t1 t2)
  assertBool "t2 == t1" (aeqConverted t2 t1)

assertNotAeq :: C.Term -> C.Term -> Assertion
assertNotAeq t1 t2 = do
  assertBool "t1 /= t2" (not (aeqConverted t1 t2))
  assertBool "t2 /= t1" (not (aeqConverted t2 t1))

-- | @I@ and @MkI :: Int -> I@
iTy :: C.Type
iTy = parseTyConTy "I"

mkI :: C.DataCon
mkI = mkDataCon "MkI" 1001 [] [] [intTy] iTy

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Alpha"
  [ testGroup "Examples of arXiv:2105.02856"
      [ testCase "eq1" $ assertAeq
          [parseToTermQQ|let (x :: Int) = exp z in x + 7|]
          [parseToTermQQ|let (y :: Int) = exp z in y + 7|]
      , testCase "eq2" $ assertAeq
          [parseToTermQQ|\(x :: Int) -> x + 7|]
          [parseToTermQQ|\(y :: Int) -> y + 7|]
      , testCase "eq3" $ assertAeq
          [parseToTermQQ|\(x :: Int) -> x + y|]
          [parseToTermQQ|\(p :: Int) -> p + y|]
      , testCase "eq4" $ assertAeq
          [parseToTermQQ|let (bar :: Int) = x + 1 in bar * y|]
          [parseToTermQQ|let (pub :: Int) = x + 1 in pub * y|]
      , testCase "eq5" $ assertAeq
          [parseToTermQQ|\(x :: Int) -> x + t|]
          [parseToTermQQ|\(x :: Int) -> x + t|]
      , testCase "neq1" $ assertNotAeq
          [parseToTermQQ|\(x :: Int) -> x + y|]
          [parseToTermQQ|\(q :: INt) -> q + z|]
      , testCase "neq2" $ assertNotAeq
          [parseToTermQQ|(x :: Int) + 2|]
          [parseToTermQQ|(y :: INt) + 2|]
      , testCase "neq3" $ assertNotAeq
          [parseToTermQQ|let (x :: Int) = e1 in let (y :: Int) = e2 in x + y|]
          [parseToTermQQ|let (y :: Int) = e2 in let (x :: Int) = e1 in x + y|]
      ]

  , testCase "bound and free variables differ" $ assertNotAeq
      [parseToTermQQ|\(x :: Int) (y :: Int) -> x|]
      [parseToTermQQ|\(x :: Int) (y :: Int) -> y|]

  , testCase "the types of binders are compared" $ assertNotAeq
      [parseToTermQQ|\(x :: Int) -> x|]
      [parseToTermQQ|\(x :: INt) -> x|]

  , testCase "a sunk term is alpha-equivalent to its freshened version" $ do
      let tm = [parseToTermQQ|\(x :: Int) -> let (y :: Int) = x in \(z :: Int) -> y|]
      extendSunk emptyConvEnv [] [] $ \env -> do
        let tm1 = sinkClosed (fromClashTermClosed tm)
        assertBool "aeq" (aeqTm tm1 (freshen (convScope env) tm1))

  , testGroup "commonAltBody"
      [ testCase "alternatives with the same right-hand side" $ do
          -- @case s of { _ -> y; MkI z -> y }@
          let s = localId C.User "s" 1 iTy
              y = intId C.User "y" 2
              z = intId C.User "z" 3
              tm = C.Case (C.Var s) intTy [(C.DefaultPat, C.Var y), (C.DataPat mkI [] [z], C.Var y)]
          withConvEnv [] [s, y] $ \env -> case fromClashTerm env tm of
            Case _ _ alts -> case commonAltBody alts of
              Just e -> assertScopedAlphaEq (C.Var y) e
              Nothing -> assertFailure "no common right-hand side"
            _ -> assertFailure "unexpected shape"

        -- @case s of { _ -> y; MkI yPat -> yPat }@, where @yPat@ has the same
        -- unique as @y@. The right-hand sides differ, as they are compared
        -- under their own binders.
      , testCase "alternatives are compared under their own binders" $ do
          let s = localId C.User "s" 7 iTy
              y = intId C.User "y" 8
              yPat = intId C.User "yPat" 8
              tm = C.Case (C.Var s) intTy
                     [(C.DefaultPat, C.Var y), (C.DataPat mkI [] [yPat], C.Var yPat)]
          withConvEnv [] [s, y] $ \env -> case fromClashTerm env tm of
            Case _ _ alts -> assertBool "different" (isNothing (commonAltBody alts))
            _ -> assertFailure "unexpected shape"
          -- The same, with the second alternative sunk into the scope of y,
          -- where its binder has the name of y
          withConvEnv [] [s] $ \env0 ->
            case fromClashTerm env0 (C.Case (C.Var s) intTy [(C.DataPat mkI [] [yPat], C.Var yPat)]) of
              Case _ _ [alt2] -> extendConvEnv env0 [] [y] $ \env1 -> do
                let alts = [Alt DefaultPat (fromClashTerm env1 (C.Var y)), sink alt2]
                assertBool "different" (isNothing (commonAltBody alts))
              _ -> assertFailure "unexpected shape"
      ]

  , testCase "aeqTy" $ do
      let a = tyVar C.User "a" 1
          b = tyVar C.User "b" 2
          conv = fromClashTypeClosed emptyScope
          idTy v = C.ForAllTy v (C.mkFunTy (C.VarTy v) (C.VarTy v))
      assertBool "forall a. a -> a == forall b. b -> b" (aeqTy (conv (idTy a)) (conv (idTy b)))
      assertBool "forall a. a -> a /= forall a. a -> Int"
        (not (aeqTy (conv (idTy a)) (conv (C.ForAllTy a (C.mkFunTy (C.VarTy a) intTy)))))
  ]
