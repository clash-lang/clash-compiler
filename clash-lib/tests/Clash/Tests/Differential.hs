{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the differential test harness, "Test.Clash.Differential"
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Differential (tests) where

import Data.Default (def)
import qualified Data.List as List
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (IntLiteral))
import qualified Clash.Core.Name as C
import Clash.Core.Pretty (showPpr)
import qualified Clash.Core.Term as C
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.Combinators (bottomupR, repeatR)
import Clash.Rewrite.Util (changed)

import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.Convert (fromClashTermClosed, toClashTerm)
import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped (ScopedRewrite, ScopedTransform (..))

import Test.Clash.Differential
import Test.Clash.Rewrite
  (globalId, intId, intLit, intTy, mkBinding, mkBindingMap, parseToTermQQ)

tests :: TestTree
tests = testGroup "Clash.Tests.Differential"
  [ testGroup "firstDiff"
      [ testCase "no difference between alpha-equivalent terms" $ do
          let t1 = closed [parseToTermQQ|
                \(x_1 :: Int) -> let { (y_2 :: Int) = x_1 } in (\(z_3 :: Int) -> z_3) y_2
              |]
              t2 = closed [parseToTermQQ|
                \(a_7 :: Int) -> let { (b_5 :: Int) = a_7 } in (\(c_9 :: Int) -> c_9) b_5
              |]
          assertBool "alpha-equivalent" (aeqTm t1 t2)
          assertNoDiff (firstDiff t1 t2)

      , testCase "the first differing subterm" $ do
          let t1 = closed [parseToTermQQ| \(f_1 :: Int) (x_2 :: Int) -> f_1 x_2 1 |]
              t2 = closed [parseToTermQQ| \(f_1 :: Int) (x_2 :: Int) -> f_1 x_2 2 |]
          d <- assertDiff (firstDiff t1 t2)
          diffPath d @?= ["Lam.body", "Lam.body", "App.arg"]
          diffCurrent d @?= showPpr (intLit 1)
          diffScoped d @?= showPpr (intLit 2)

      , testCase "bound variables are compared by their binders" $ do
          let t1 = closed [parseToTermQQ| \(x_1 :: Int) (y_2 :: Int) -> x_1 |]
              t2 = closed [parseToTermQQ| \(x_1 :: Int) (y_2 :: Int) -> y_2 |]
          d <- assertDiff (firstDiff t1 t2)
          diffPath d @?= ["Lam.body", "Lam.body"]

      , testCase "the types of binders" $ do
          let t1 = closed [parseToTermQQ| \(x_1 :: Int) -> x_1 |]
              t2 = closed [parseToTermQQ| \(x_1 :: Bool) -> x_1 |]
          d <- assertDiff (firstDiff t1 t2)
          diffPath d @?= ["Lam.type"]

      , testCase "a different number of alternatives is a difference of the case" $ do
          let x = intId C.User "x" 1
              cs alts = closed (C.Lam x (C.Case (C.Var x) intTy alts))
              t1 = cs [(C.DefaultPat, intLit 1)]
              t2 = cs [(C.LitPat (IntLiteral 0), intLit 0), (C.DefaultPat, intLit 1)]
          d <- assertDiff (firstDiff t1 t2)
          diffPath d @?= ["Lam.body"]
      ]

  , testGroup "known differences"
      [ testCase "rec-to-nonrec turns a non-recursive Rec into a NonRec" $ do
          let x = intId C.User "x" 1
              y = intId C.User "y" 2
              nonRec = closed (C.Lam x (C.Let (C.NonRec y (C.Var x)) (C.Var y)))
              rec_ = closed (C.Lam x (C.Let (C.Rec [(y, C.Var x)]) (C.Var y)))
          assertBool "different before" (not (aeqTm nonRec rec_))
          assertBool "equal after" (aeqTm nonRec (kdNormalize recToNonRec emptyScope rec_))

      , testCase "letrec-order orders bindings by their first reference" $ do
          let x = intId C.User "x" 1
              y = intId C.User "y" 2
              z = intId C.User "z" 3
              -- letrec y = x; z = y in z, in two orders
              t1 = closed (C.Lam x (C.Let (C.Rec [(y, C.Var x), (z, C.Var y)]) (C.Var z)))
              t2 = closed (C.Lam x (C.Let (C.Rec [(z, C.Var y), (y, C.Var x)]) (C.Var z)))
          assertBool "different before" (not (aeqTm t1 t2))
          assertBool "equal after"
            (aeqTm (kdNormalize letrecOrder emptyScope t1) (kdNormalize letrecOrder emptyScope t2))

      , testCase "rec-to-nonrec keeps a recursive Rec" $ do
          let y = intId C.User "y" 2
              rec_ = closed (C.Let (C.Rec [(y, C.Var y)]) (C.Var y))
          case kdNormalize recToNonRec emptyScope rec_ of
            Let (Rec _ _) _ -> pure ()
            _ -> assertFailure "not a Rec"
      ]

  , testGroup "compareBinding"
      [ testCase "the conversion round trip is the identity" $ do
          o <- compareOn (roundTripCmp []) letTerm
          assertEqualOutcome o

      , testCase "a broken port is different" $ do
          o <- compareOn brokenCmp letTerm
          case o of
            Different (Mismatch (Just d) _ _) -> do
              diffPath d @?= ["Lam.body", "Letrec.rhs[1]"]
              diffCurrent d @?= showPpr (intLit 3)
              diffScoped d @?= showPpr (intLit 4)
            _ -> assertFailure ("expected a mismatch, got: " <> showOutcome o)

      , testCase "an intended difference is different, unless it is known" $ do
          o <- compareOn (nonRecCmp []) recTerm
          case o of
            Different Mismatch {} -> pure ()
            _ -> assertFailure ("expected a mismatch, got: " <> showOutcome o)
          o1 <- compareOn (nonRecCmp [recToNonRec]) recTerm
          case o1 of
            KnownDifferent ["rec-to-nonrec"] -> pure ()
            _ -> assertFailure ("expected known-different, got: " <> showOutcome o1)

      , testCase "a crash of a side is reported" $ do
          let cmp = Comparison "crash" "" (pureSide id) (pureSide (error "boom")) []
          o <- compareOn cmp letTerm
          case o of
            Different (Crash stage msg) -> do
              stage @?= "scoped side"
              assertBool msg ("boom" `List.isInfixOf` msg)
            _ -> assertFailure ("expected a crash, got: " <> showOutcome o)
      ]

  , testGroup "running transformations"
      [ testCase "Once applies the transformation once at every node" $ do
          o <- compareOn (decrCmp Once Once) decrTerm
          assertEqualOutcome o
          -- Every literal is decremented once
          o1 <- compareOn (Comparison "" "" (pureSide id) (scopedRewrite Once decrS) []) decrTerm
          case o1 of
            Different (Mismatch (Just d) _ _) -> diffScoped d @?= showPpr (intLit 1)
            _ -> assertFailure ("expected a mismatch, got: " <> showOutcome o1)

      , testCase "Fixpoint applies the transformation until it no longer fires" $ do
          o <- compareOn (decrCmp Fixpoint Fixpoint) decrTerm
          assertEqualOutcome o
          o1 <- compareOn (decrCmp Once Fixpoint) decrTerm
          case o1 of
            Different (Mismatch (Just d) c s) -> do
              diffCurrent d @?= showPpr (intLit 1)
              diffScoped d @?= showPpr (intLit 0)
              -- All literals are 0
              assertBool (showPpr s) (not ("1" `List.isInfixOf` showPpr s))
              assertBool (showPpr c) ("1" `List.isInfixOf` showPpr c)
            _ -> assertFailure ("expected a mismatch, got: " <> showOutcome o1)

      , testCase "the scoped traversal goes under binders" $ do
          o <- compareOn (decrCmp Fixpoint Fixpoint) letTerm
          assertEqualOutcome o
      ]
  ]
 where
  closed = fromClashTermClosed

  letTerm = [parseToTermQQ|
    \(x_1 :: Int) -> let { (y_2 :: Int) = x_1; (z_3 :: Int) = 3 } in (\(w_4 :: Int) -> w_4) y_2
  |]

  decrTerm = [parseToTermQQ| \(f_1 :: Int) -> f_1 2 (f_1 1 (\(x_2 :: Int) -> 3)) |]

  recTerm = [parseToTermQQ| \(x_1 :: Int) -> let { (y_2 :: Int) = x_1 } in y_2 |]

  -- The scoped side replaces every literal 3 by a 4
  brokenCmp = Comparison "broken" "" (pureSide id) (pureSide (everywhereTm threeIsFour)) []
   where
    threeIsFour :: Term n -> Term n
    threeIsFour = \case
      Literal (IntLiteral 3) -> Literal (IntLiteral 4)
      e -> e

  -- The scoped side turns non-recursive Recs into NonRecs
  nonRecCmp = Comparison "nonrec" "" (pureSide id) (pureSide (kdNormalize recToNonRec emptyScope))

  roundTripCmp = Comparison "round trip" ""
    (pureSide (toClashTerm . fromClashTermClosed))
    (pureSide (fromClashTermClosed . toClashTerm))

  decrC :: NormRewrite
  decrC _ = \case
    C.Literal (IntLiteral n) | n > 0 -> changed (C.Literal (IntLiteral (n - 1)))
    e -> pure e

  decrS :: ScopedRewrite extra
  decrS = ScopedTransform $ \_ -> \case
    Literal (IntLiteral n) | n > 0 -> changed (Literal (IntLiteral (n - 1)))
    e -> pure e

  decrCmp :: Mode -> Mode -> Comparison
  decrCmp mc ms = Comparison "decrement" ""
    (currentRewrite "decrement" (case mc of
       Once -> bottomupR decrC
       Fixpoint -> repeatR (bottomupR decrC)))
    (scopedRewrite ms decrS)
    []

  compareOn :: Comparison -> C.Term -> IO Outcome
  compareOn cmp tm = do
    let f = globalId C.User "f" 100 intTy
    d <- mkDesign def (mkBindingMap [(f, tm)])
    compareBinding cmp d (mkBinding f tm)

  assertEqualOutcome = \case
    Equal -> pure ()
    o -> assertFailure ("expected equal, got: " <> showOutcome o)

  assertNoDiff = \case
    Nothing -> pure ()
    Just d -> assertFailure ("unexpected difference at " <> show (diffPath d))

  assertDiff = \case
    Nothing -> assertFailure "no difference found"
    Just d -> pure d

showOutcome :: Outcome -> String
showOutcome = \case
  Equal -> "equal"
  KnownDifferent kds -> "known-different: " <> show kds
  Different m -> "different:\n" <> renderMismatch 20 100 (intId C.User "f" 100) m
