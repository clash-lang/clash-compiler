{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for alpha-equivalence of scoped Clash Core.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Alpha (tests) where

import qualified Data.List as List
import Data.Maybe (isJust, isNothing)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Clash.Core.Alpha
import Clash.Core.Subst
import Clash.Core.Syntax

import Test.Clash.Scoped
  ( TestTerm, assertTermIs, closedType, extendTestScope, forAllScrambled, sunkD, termIn
  , testScope, varD, withClashing, withTerm, withTerms, withTestScope, withTests)
import Test.Clash.Scoped.Gen
  ( InScope (..), Sunk (..), genInScope, genPerturbed, genSameRhsCase, genSunk, rawEqTm
  , shrinkInScope, shrinkSunk, showT)

-- | Assert that two terms, built in the scope of their free variables, are
-- alpha-equivalent (or not), both ways
assertAeq, assertNotAeq :: TestTerm -> TestTerm -> Assertion
assertAeq t1 t2 = withTerms [t1, t2] $ \_ -> \case
  [e1, e2] -> do
    assertBool "t1 == t2" (aeqTm e1 e2)
    assertBool "t2 == t1" (aeqTm e2 e1)
  _ -> assertFailure "impossible"
assertNotAeq t1 t2 = withTerms [t1, t2] $ \_ -> \case
  [e1, e2] -> do
    assertBool "t1 /= t2" (not (aeqTm e1 e2))
    assertBool "t2 /= t1" (not (aeqTm e2 e1))
  _ -> assertFailure "impossible"

-- | Whether two terms in a scope are alpha-equivalent, by an independent
-- oracle: whether they are the same after numbering their binders by
-- position, see 'rawEqTm'
aeqOracle :: Distinct n => Scope n -> Term n -> Term n -> Bool
aeqOracle sc t1 t2 = rawEqTm (freshenDistinct sc t1) (freshenDistinct sc t2)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Alpha"
  [ testGroup "Examples of arXiv:2105.02856"
      [ testCase "eq1" $ assertAeq
          "let (x :: Int) = exp z in x + 7"
          "let (y :: Int) = exp z in y + 7"
      , testCase "eq2" $ assertAeq
          "\\(x :: Int) -> x + 7"
          "\\(y :: Int) -> y + 7"
      , testCase "eq3" $ assertAeq
          "\\(x :: Int) -> x + y"
          "\\(p :: Int) -> p + y"
      , testCase "eq4" $ assertAeq
          "let (bar :: Int) = x + 1 in bar * y"
          "let (pub :: Int) = x + 1 in pub * y"
      , testCase "eq5" $ assertAeq
          "\\(x :: Int) -> x + t"
          "\\(x :: Int) -> x + t"
      , testCase "neq1" $ assertNotAeq
          "\\(x :: Int) -> x + y"
          "\\(q :: INt) -> q + z"
      , testCase "neq2" $ assertNotAeq
          "(x :: Int) + 2"
          "(y :: INt) + 2"
      , testCase "neq3" $ assertNotAeq
          "let (x :: Int) = e1 in let (y :: Int) = e2 in x + y"
          "let (y :: Int) = e2 in let (x :: Int) = e1 in x + y"
      ]

  , testCase "bound and free variables differ" $ assertNotAeq
      "\\(x :: Int) (y :: Int) -> x"
      "\\(x :: Int) (y :: Int) -> y"

  , testCase "the types of binders are compared" $ assertNotAeq
      "\\(x :: Int) -> x"
      "\\(x :: INt) -> x"

  , testCase "a sunk term is alpha-equivalent to its freshened version" $
      withTerm "\\(x :: Int) -> let { !(y :: Int) = x } in \\(z :: Int) -> y" $ \ts0 tm0 ->
        withClashing ts0 [sunkD] tm0 $ \ts tm1 ->
          assertBool "aeq" (aeqTm tm1 (freshen (testScope ts) tm1))

  , testGroup "commonAltBody"
      [ testCase "alternatives with the same right-hand side" $
          withTerm "(case (s :: Pair) of { _ -> (y :: Int); MkPair (z :: Int) (w :: Int) -> y }) :: Int" $ \ts -> \case
            Case _ _ alts -> case commonAltBody alts of
              Just e -> assertTermIs ts "y" e
              Nothing -> assertFailure "no common right-hand side"
            _ -> assertFailure "unexpected shape"

        -- @case s of { _ -> y; MkPair y z -> y }@, where the second @y@ is
        -- bound by the pattern. The right-hand sides differ, as they are
        -- compared under their own binders.
      , testCase "alternatives are compared under their own binders" $ do
          withTerm "(case (s :: Pair) of { _ -> (y :: Int); MkPair (y :: Int) (z :: Int) -> y }) :: Int" $ \_ -> \case
            Case _ _ alts -> assertBool "different" (isNothing (commonAltBody alts))
            _ -> assertFailure "unexpected shape"
          -- The same, with the second alternative built in the scope of s,
          -- where its first binder gets name 2, and sunk into the scope of a
          -- variable y, which gets name 2 too
          withTestScope [varD "s" "Pair"] $ \ts0 ->
            case termIn ts0 "(case s of { MkPair (yPat :: Int) (z :: Int) -> yPat }) :: Int" of
              Case _ _ [alt2] -> extendTestScope ts0 [varD "y" "Int"] $ \ts1 -> do
                let alts = [Alt DefaultPat (termIn ts1 "y"), sink alt2]
                assertBool "different" (isNothing (commonAltBody alts))
              _ -> assertFailure "unexpected shape"
      ]

  , testCase "acmpTy" $ do
      let idA = closedType "forall a. a -> a"
          idB = closedType "forall b. b -> b"
          constA = closedType "forall a. a -> Int"
          constB = closedType "forall b. b -> Int"
      acmpTy idA idB @?= EQ
      acmpTy idA constA @?= invert (acmpTy constB idB)
      assertBool "different" (acmpTy idA constA /= EQ)

  , testCase "aeqTy" $ do
      assertBool "forall a. a -> a == forall b. b -> b"
        (aeqTy (closedType "forall a. a -> a") (closedType "forall b. b -> b"))
      assertBool "forall a. a -> a /= forall a. a -> Int"
        (not (aeqTy (closedType "forall a. a -> a") (closedType "forall a. a -> Int")))

  , withTests 500 $ testGroup "properties"
      [ testProperty "a term is alpha-equivalent to itself with scrambled binders" $
          forAllShrink genInScope shrinkInScope $ \(InScope ts tm) ->
            forAllScrambled (testScope ts) tm $ \tm2 ->
              counterexample "aeqTm" (aeqTm tm tm2) .&&. counterexample "aeqTm, flipped" (aeqTm tm2 tm)

        -- The changes may make the terms alpha-inequivalent, or not
      , testProperty "aeqTm agrees with comparing numbered binders on changed terms" $
          forAllShrink genInScope shrinkInScope $ \(InScope ts tm) ->
            forAll (genPerturbed ts tm) $ \tm2 ->
              counterexample ("changed:\n" <> showT tm2) $ do
                let expected = aeqOracle (testScope ts) tm tm2
                classify expected "alpha-equivalent" $
                  aeqTm tm tm2 === expected .&&.
                  aeqTm tm2 tm === expected

      , testProperty "aeqTm agrees with comparing numbered binders on changed terms with scrambled binders" $
          forAllShrink genSunk shrinkSunk $ \(Sunk ts0 ts1 tm) ->
            forAll (genPerturbed ts0 tm) $ \tm2 ->
              counterexample ("changed:\n" <> showT tm2) $ do
                let expected = aeqOracle (testScope ts0) tm tm2
                    sc = testScope ts1
                forAllScrambled sc (sink tm) $ \s1 ->
                  forAllScrambled sc (sink tm2) $ \s2 ->
                    classify expected "alpha-equivalent" $
                      aeqTm s1 s2 === expected .&&. aeqTm s2 s1 === expected

      , testProperty "a sunk term is alpha-equivalent to its freshened version" $
          forAllShrink genSunk shrinkSunk $ \(Sunk _ ts1 tm) -> do
            let tm1 = sink tm
                tm2 = freshen (testScope ts1) tm1
            aeqTm tm1 tm2 .&&. aeqTm tm2 tm1

      , testProperty "acmpTm agrees with aeqTm on changed terms with scrambled binders" $
          forAllShrink genSunk shrinkSunk $ \(Sunk ts0 ts1 tm) ->
            forAll (genPerturbed ts0 tm) $ \tm2 ->
              counterexample ("changed:\n" <> showT tm2) $ do
                let sc = testScope ts1
                forAllScrambled sc (sink tm) $ \s1 ->
                  forAllScrambled sc (sink tm2) $ \s2 ->
                    let expected = aeqTm s1 s2
                    in  classify expected "alpha-equivalent" $
                          (acmpTm s1 s2 == EQ) === expected .&&.
                          acmpTm s1 s2 === invert (acmpTm s2 s1)

      , testProperty "acmpTm is transitive" $
          forAllShrink genInScope shrinkInScope $ \(InScope ts tm) ->
            forAll (genPerturbed ts tm) $ \tm2 ->
              forAll (genPerturbed ts tm) $ \tm3 -> do
                let sorted = List.sortBy acmpTm [tm, tm2, tm3]
                -- With an intransitive order, some sorting of the three
                -- would have a pair in the wrong order
                conjoin [ acmpTm a b =/= GT | (a : bs) <- List.tails sorted, b <- bs ]

        -- The right-hand sides are a common right-hand side if they are
        -- alpha-equivalent, and the patterns bind none of their free variables
      , testProperty "commonAltBody agrees with moving the alternatives out of their patterns" $
          forAllShrink genSameRhsCase shrinkInScope $ \(InScope ts tm) ->
            forAllScrambled (testScope ts) tm $ \case
              Case _ _ alts@(_ : _) -> do
                let unsunk = [unsinkAlt p e | Alt p e <- alts]
                    expected = case sequence unsunk of
                      Just (e0 : es) -> all (aeqOracle (testScope ts) e0) es
                      _ -> False
                    actual = commonAltBody alts
                classify expected "common right-hand side" $
                  isJust actual === expected .&&.
                    case (actual, sequence unsunk) of
                      (Just e, Just (e0 : _)) -> counterexample "aeqTm" (aeqTm e0 e)
                      _ -> property True
              _ -> property False
      ]
  ]

-- | The reverse of an ordering
invert :: Ordering -> Ordering
invert o = compare EQ o
