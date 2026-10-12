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

import qualified Data.List as List
import Data.Maybe (isJust, isNothing)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Subst as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C

import Clash.Core.Scoped.Alpha
import Clash.Core.Scoped.Convert hiding (mkDataCon)
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite
  (intId, intTy, localId, mkDataCon, parseToTermQQ, parseTyConTy, tyVar)
import Test.Clash.Scoped
  ( assertScopedAlphaEq, extendFreeVars, extendSunk, forAllScrambled, sinkInto
  , withFreeVars, withFreeVarsOf, withTests)
import Test.Clash.Scoped.Gen
  ( InScope (..), Sunk (..), genInScope, genPerturbed, genSameRhsCase, genSunk
  , patBindersFreeIn, renameBinders, shrinkInScope, shrinkSunk, showTerm)

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

  , testCase "acmpTy" $ do
      let a = tyVar C.User "a" 1
          b = tyVar C.User "b" 2
          conv = fromClashTypeClosed emptyScope
          idTy v = C.ForAllTy v (C.mkFunTy (C.VarTy v) (C.VarTy v))
          constTy v = C.ForAllTy v (C.mkFunTy (C.VarTy v) intTy)
      acmpTy (conv (idTy a)) (conv (idTy b)) @?= EQ
      acmpTy (conv (idTy a)) (conv (constTy a)) @?= invert (acmpTy (conv (constTy b)) (conv (idTy b)))
      assertBool "different" (acmpTy (conv (idTy a)) (conv (constTy a)) /= EQ)

  , testCase "aeqTy" $ do
      let a = tyVar C.User "a" 1
          b = tyVar C.User "b" 2
          conv = fromClashTypeClosed emptyScope
          idTy v = C.ForAllTy v (C.mkFunTy (C.VarTy v) (C.VarTy v))
      assertBool "forall a. a -> a == forall b. b -> b" (aeqTy (conv (idTy a)) (conv (idTy b)))
      assertBool "forall a. a -> a /= forall a. a -> Int"
        (not (aeqTy (conv (idTy a)) (conv (C.ForAllTy a (C.mkFunTy (C.VarTy a) intTy)))))

  , withTests 500 $ testGroup "properties"
      [ testProperty "aeqTm agrees with aeqTerm on renamed terms" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) -> do
            let tm2 = renameBinders tm
            counterexample ("renamed:\n" <> showTerm tm2) $
              withFreeVars fvs $ \env -> do
                let conv = fromClashTerm env
                counterexample "aeqTerm" (C.aeqTerm tm tm2) .&&.
                  counterexample "aeqTm" (aeqTm (conv tm) (conv tm2))

        -- The changes may make the terms alpha-inequivalent, or not
      , testProperty "aeqTm agrees with aeqTerm on changed terms" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
            forAll (genPerturbed fvs tm) $ \tm2 ->
              counterexample ("changed:\n" <> showTerm tm2) $
                withFreeVars fvs $ \env -> do
                  let conv = fromClashTerm env
                      expected = C.aeqTerm tm tm2
                  classify expected "alpha-equivalent" $
                    aeqTm (conv tm) (conv tm2) === expected .&&.
                    aeqTm (conv tm2) (conv tm) === expected

      , testProperty "aeqTm agrees with aeqTerm on changed terms with scrambled binders" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) ->
            forAll (genPerturbed fvs0 tm) $ \tm2 ->
              counterexample ("changed:\n" <> showTerm tm2) $
                withFreeVars fvs0 $ \env0 -> do
                  let c1 = fromClashTerm env0 tm
                      c2 = fromClashTerm env0 tm2
                      expected = C.aeqTerm tm tm2
                  extendFreeVars env0 fvs1 $ \env1 -> do
                    let sc = convScope env1
                    forAllScrambled sc (sinkInto sc c1) $ \s1 ->
                      forAllScrambled sc (sinkInto sc c2) $ \s2 ->
                        classify expected "alpha-equivalent" $
                          aeqTm s1 s2 === expected .&&. aeqTm s2 s1 === expected

      , testProperty "a sunk term is alpha-equivalent to the term converted in the larger scope" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) ->
            withFreeVars fvs0 $ \env0 -> do
              let tm0 = fromClashTerm env0 tm
              extendFreeVars env0 fvs1 $ \env1 -> do
                let tm1 = sinkInto (convScope env1) tm0
                    tm2 = fromClashTerm env1 tm
                aeqTm tm1 tm2 .&&. aeqTm tm2 tm1

      , testProperty "acmpTm agrees with aeqTm on changed terms with scrambled binders" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) ->
            forAll (genPerturbed fvs0 tm) $ \tm2 ->
              counterexample ("changed:\n" <> showTerm tm2) $
                withFreeVars fvs0 $ \env0 -> do
                  let c1 = fromClashTerm env0 tm
                      c2 = fromClashTerm env0 tm2
                  extendFreeVars env0 fvs1 $ \env1 -> do
                    let sc = convScope env1
                    forAllScrambled sc (sinkInto sc c1) $ \s1 ->
                      forAllScrambled sc (sinkInto sc c2) $ \s2 ->
                        let expected = aeqTm s1 s2
                        in  classify expected "alpha-equivalent" $
                              (acmpTm s1 s2 == EQ) === expected .&&.
                              acmpTm s1 s2 === invert (acmpTm s2 s1)

      , testProperty "acmpTm is transitive" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
            forAll (genPerturbed fvs tm) $ \tm2 ->
              forAll (genPerturbed fvs tm) $ \tm3 ->
                withFreeVars fvs $ \env -> do
                  let c1 = fromClashTerm env tm
                      c2 = fromClashTerm env tm2
                      c3 = fromClashTerm env tm3
                      sorted = List.sortBy acmpTm [c1, c2, c3]
                  -- With an intransitive order, some sorting of the three
                  -- would have a pair in the wrong order
                  conjoin [ acmpTm a b =/= GT | (a : bs) <- List.tails sorted, b <- bs ]

        -- The right-hand sides are a common right-hand side if they are
        -- alpha-equivalent, and the patterns bind none of their free variables
      , testProperty "commonAltBody agrees with Clash" $
          forAllShrink genSameRhsCase shrinkInScope $ \(InScope fvs tm) ->
            withFreeVars fvs $ \env ->
              forAllScrambled (convScope env) (fromClashTerm env tm) $ \tm1 ->
                case (tm, tm1) of
                  (C.Case _ _ alts@((_, rhs0) : _), Case _ _ alts1) -> do
                    let expected =
                          all (\(_, rhs) -> C.aeqTerm rhs0 rhs) alts &&
                            not (any (uncurry patBindersFreeIn) alts)
                        actual = commonAltBody alts1
                    classify expected "common right-hand side" $
                      isJust actual === expected .&&.
                        case actual of
                          Just e -> counterexample "aeqTerm" (C.aeqTerm rhs0 (toClashTerm e))
                          Nothing -> property True
                  _ -> property False
      ]
  ]

-- | The reverse of an ordering
invert :: Ordering -> Ordering
invert o = compare EQ o
