{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Scoped.WorkFree", against "Clash.Rewrite.WorkFree"
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Rewrite.Scoped.WorkFree (tests) where

import Control.Monad.Trans.State.Strict (evalState, runState)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Var as C
import Clash.Core.VarEnv (emptyVarEnv)
import qualified Clash.Normalize.Primitives as C (removedArg)
import qualified Clash.Rewrite.WorkFree as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.NameMap (emptyGlobalEnv, lookupGlobalEnv)
import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped.WorkFree

import Test.Clash.Rewrite
  ( clockTy, globalId, intFunTy, intId, intLit, intTy, localId, mkBindingMap
  , parseToTermQQ, testTyConMap )
import Test.Clash.Scoped (mkScopedBindingMap, withFreeVarsOf)

-- | Global bindings: @lit = 1@, @work = p lit@ with a primitive @p@ that
-- always does work, and @loop = loop@
bindings :: [(C.Id, C.Term)]
bindings =
  [ (litG, intLit 1)
  , (workG, C.App (prim C.WorkAlways) (C.Var litG))
  , (loopG, C.Var loopG) ]

litG, workG, loopG, funG :: C.Id
litG = globalId C.User "lit" 200 intTy
workG = globalId C.User "work" 201 intTy
loopG = globalId C.User "loop" 202 intTy
funG = globalId C.User "fun" 203 (intFunTy 1)

-- | A primitive @Int -> Int@ with the given work info
prim :: C.WorkInfo -> C.Term
prim wi = C.Prim (C.PrimInfo "p" (intFunTy 1) wi C.SingleResult C.NoUnfolding)

x :: C.Term
x = C.Var (intId C.User "x" 1)

-- | A local function @f :: Int -> Int@
f :: C.Term
f = C.Var (localId C.User "f" 2 (intFunTy 1))

-- | A local clock
clk :: C.Term
clk = C.Var (localId C.User "clk" 3 clockTy)

examples :: [C.Term]
examples =
  [ x
  , f
  , C.App f x
  , C.Var litG
  , C.Var workG
  , C.Var loopG
  , C.Var funG
  , C.App (prim C.WorkNever) x
  , C.App (prim C.WorkVariable) x
  , C.App (prim C.WorkVariable) (intLit 3)
  , C.App (prim C.WorkConstant) (C.Var workG)
  , C.App (prim C.WorkAlways) (intLit 3)
  , [parseToTermQQ| \(y :: Int) -> y |]
  , C.Lam (intId C.User "y" 4) x
  , C.Let (C.NonRec (intId C.User "y" 4) (C.Var workG)) x
  , C.Tick C.DeDup (C.Var litG)
  , clk
  , C.Prim C.removedArg
  ]

-- | Assert that a function on scoped terms agrees with one on Clash terms on
-- all 'examples'
assertAgreesOnExamples
  :: (Eq a, Show a)
  => (C.Term -> a) -> (forall n. Distinct n => Scope n -> Term n -> a) -> Assertion
assertAgreesOnExamples fC gS =
  sequence_
    [ withFreeVarsOf [tm] $ \env -> (show tm, gS (convScope env) (fromClashTerm env tm)) @?= (show tm, fC tm)
    | tm <- examples ]

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Scoped.WorkFree"
  [ testCase "isWorkFree" $
      assertAgreesOnExamples
        (\tm -> evalState (C.isWorkFree id (mkBindingMap bindings') tm) emptyVarEnv)
        (\_ tm -> evalState (isWorkFree id (mkScopedBindingMap bindings') tm) emptyGlobalEnv)
  , testCase "isWorkFree caches globals" $ do
      let check = isWorkFree id (mkScopedBindingMap bindings') . fromClashTermClosed
          (_, cache) = runState
            (check (C.App (prim C.WorkNever) (C.Var litG)) >> check (C.Var workG))
            emptyGlobalEnv
      lookupGlobalEnv (mkGlobalId workG) cache @?= Just False
      lookupGlobalEnv (mkGlobalId litG) cache @?= Just True
  , testCase "isConstant" $
      assertAgreesOnExamples C.isConstant (const isConstant)
  , testCase "isConstantNotClockReset" $
      assertAgreesOnExamples
        (C.isConstantNotClockReset testTyConMap)
        (isConstantNotClockReset testTyConMap)
  , testCase "isWorkFreeClockOrResetOrEnable" $
      assertAgreesOnExamples
        (C.isWorkFreeClockOrResetOrEnable testTyConMap)
        (isWorkFreeClockOrResetOrEnable testTyConMap)
  , testCase "isWorkFreeIsh" $
      assertAgreesOnExamples (C.isWorkFreeIsh testTyConMap) (isWorkFreeIsh testTyConMap)
  ]
 where
  bindings' = (funG, C.Lam (intId C.User "z" 5) (C.Var (intId C.User "z" 5))) : bindings
