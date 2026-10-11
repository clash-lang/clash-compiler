{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.TermInfo", against "Clash.Core.TermInfo"
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Core.Scoped.TermInfo (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.HasFreeVars as C (isClosed)
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.TermInfo as C
import qualified Clash.Core.Type as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TermInfo

import Test.Clash.Rewrite
  ( globalId, intId, intTy, localId, parseToTermQQ, testTyConMap, tyVar )
import Test.Clash.Scoped (assertScopedAlphaEq, withFreeVarsOf)

-- | Terms of all shapes
examples :: [C.Term]
examples =
  [ [parseToTermQQ| \(x :: Int) (y :: Int) -> x |]
  , [parseToTermQQ| \(x :: Int) -> let (y :: Int) = x in y |]
  , [parseToTermQQ| \(x :: Int) -> let { (y :: Int) = f y; (z :: Int) = y } in z |]
  , C.TyLam a (C.Lam ax (C.Var ax))
  , C.Var x
  , C.Var g
  , C.App (C.Var g) (C.Var x)
  , C.Tick C.DeDup (C.Var x)
  , C.Cast (C.Var x) intTy intTy
  , C.Lam ax (C.Var x)
  ]
 where
  a = tyVar C.User "a" 50
  ax = localId C.User "ax" 51 (C.VarTy a)
  x = intId C.User "x" 52
  g = globalId C.User "g" 53 (C.mkFunTy intTy intTy)

-- | Assert that a function on scoped terms agrees with one on Clash terms on
-- all 'examples'
assertAgreesOnExamples
  :: (Eq a, Show a)
  => (C.Term -> a) -> (forall n. Distinct n => Scope n -> Term n -> a) -> Assertion
assertAgreesOnExamples f g =
  sequence_
    [ withFreeVarsOf [tm] $ \env -> g (convScope env) (fromClashTerm env tm) @?= f tm
    | tm <- examples ]

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.TermInfo"
  [ testCase "termSize" $ assertAgreesOnExamples C.termSize (const termSize)
  , testCase "termSizeSmallerThan" $
      sequence_
        [ assertAgreesOnExamples (C.termSizeSmallerThan k) (const (termSizeSmallerThan k))
        | k <- [0..6] ]
  , testCase "isFun" $
      assertAgreesOnExamples (C.isFun testTyConMap) (isFun testTyConMap)
  , testCase "isPolyFun" $
      assertAgreesOnExamples (C.isPolyFun testTyConMap) (isPolyFun testTyConMap)
  , testCase "isLet" $ assertAgreesOnExamples C.isLet (const isLet)
  , testCase "isVar" $ assertAgreesOnExamples C.isVar (const isVar)
  , testCase "isLocalVar" $ assertAgreesOnExamples C.isLocalVar (const isLocalVar)
  , testCase "isGlobalVar" $
      assertAgreesOnExamples
        (\case C.Var v -> not (C.isLocalVar (C.Var v)); _ -> False)
        (const isGlobalVar)
  , testCase "isCon, isPrim, isTick, isCast" $ do
      assertAgreesOnExamples C.isCon (const isCon)
      assertAgreesOnExamples C.isPrim (const isPrim)
      assertAgreesOnExamples C.isTick (const isTick)
      assertAgreesOnExamples C.isCast (const isCast)
  , testCase "isClosed" $
      assertAgreesOnExamples C.isClosed (const isClosed)
  , testCase "splitMultiPrimArgs" $ do
      let mpi = C.MultiPrimInfo undefined undefined [intTy, intTy]
          r1 = intId C.User "r1" 1
          r2 = intId C.User "r2" 2
          i = intId C.User "i" 3
          args = [Right intTy, Left (C.Var i), Left (C.Var r1), Left (C.Var r2)]
          (argsC, resC) = C.splitMultiPrimArgs mpi args
      withFreeVarsOf [C.Var r1, C.Var r2, C.Var i] $ \env -> do
        let (args1, res) =
              splitMultiPrimArgs mpi
                (map (either (Left . fromClashTerm env) (Right . fromClashType env)) args)
        length args1 @?= length argsC
        length res @?= length resC
        sequence_ [assertScopedAlphaEq (C.Var v) t | (v, t) <- zip resC res]
  ]
