{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Scoped.WorkFree". The expected results are those of
  the functions of "Clash.Rewrite.WorkFree" that these replace.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Rewrite.Scoped.WorkFree (tests) where

import Control.Monad.Trans.State.Strict (evalState, runState)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Term as C (IsMultiPrim (SingleResult), WorkInfo (..))

import Clash.Core.Scoped.Builder (TmB, appB, deDupTickB, primB, tickB)
import Clash.Core.Scoped.Convert (mkPrimInfo)
import Clash.Core.Scoped.NameMap (emptyGlobalEnv, lookupGlobalEnv)
import Clash.Core.Scoped.Syntax
import qualified Clash.Core.Scoped.TysPrim as P
import Clash.Rewrite.Scoped.WorkFree

import Test.Clash.Scoped

-- | Global bindings: @lit = 1@, @work = p lit@ with a primitive @p@ that
-- always does work, @loop = loop@ and @fun = \\z -> z@
bindings :: [(GlobalId, TestTerm)]
bindings =
  [ (testGlobalId "lit_G200" "Int", "1")
  , (testGlobalId "work_G201" "Int", wrapB (appB (prim C.WorkAlways)) "lit_G200 :: Int")
  , (testGlobalId "loop_G202" "Int", "(loop_G202 :: Int)")
  , (testGlobalId "fun_G203" "Int -> Int", "\\(z :: Int) -> z") ]

-- | A primitive @Int -> Int@ with the given work info
prim :: C.WorkInfo -> TmB
prim wi = primB (mkPrimInfo "p" (closedType "Int -> Int") wi C.SingleResult NoUnfolding)

-- | A term made with the builders, around the given parsed one
wrapB :: (TmB -> TmB) -> TestTerm -> TestTerm
wrapB f t = t {testTermB = f (testTermB t)}

-- | @removedArg@ of "Clash.Normalize.Primitives"
removedArg :: PrimInfo
removedArg =
  mkPrimInfo "Clash.Normalize.Primitives.removedArg" P.undefinedTy C.WorkNever C.SingleResult NoUnfolding

examples :: [TestTerm]
examples =
  [ "(x :: Int)"
  , "(f :: Int -> Int)"
  , "(f :: Int -> Int) (x :: Int)"
  , "(lit_G200 :: Int)"
  , "(work_G201 :: Int)"
  , "(loop_G202 :: Int)"
  , "(fun_G203 :: Int -> Int)"
  , wrapB (appB (prim C.WorkNever)) "(x :: Int)"
  , wrapB (appB (prim C.WorkVariable)) "(x :: Int)"
  , wrapB (appB (prim C.WorkVariable)) "3"
  , wrapB (appB (prim C.WorkConstant)) "(work_G201 :: Int)"
  , wrapB (appB (prim C.WorkAlways)) "3"
  , "\\(y :: Int) -> y"
  , "\\(y :: Int) -> (x :: Int)"
  , "let { !(y :: Int) = (work_G201 :: Int) } in (x :: Int)"
  , wrapB (tickB deDupTickB) "(lit_G200 :: Int)"
  , "(clk :: Clock \"System\")"
  , builtTerm (primB removedArg)
  ]

-- | Assert that a function gives the expected results on 'examples', each
-- built in the scope of its free variables
assertResults
  :: (Eq a, Show a) => (forall n. Distinct n => Scope n -> Term n -> a) -> [a] -> Assertion
assertResults f expected =
  sequence_
    [ withTerm tm $ \ts e -> assertEqual ("example " <> show i) ex (f (testScope ts) e)
    | (i, tm, ex) <- zip3 [0 :: Int ..] examples expected ]

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Scoped.WorkFree"
  [ testCase "isWorkFree" $
      assertResults (\_ tm -> evalState (isWorkFree id (mkGlobalBindings bindings) tm) emptyGlobalEnv)
        [ True, True, False, True, False, False, False, True, False, True, True, False, True, True
        , False, True, True, True ]
  , testCase "isWorkFree caches globals" $ do
      let check = isWorkFree id (mkGlobalBindings bindings) . closedTerm
          (_, cache) = runState
            (check (wrapB (appB (prim C.WorkNever)) "(lit_G200 :: Int)") >> check "(work_G201 :: Int)")
            emptyGlobalEnv
      lookupGlobalEnv (testGlobalId "work_G201" "Int") cache @?= Just False
      lookupGlobalEnv (testGlobalId "lit_G200" "Int") cache @?= Just True
  , testCase "isConstant" $
      assertResults (const isConstant)
        [ False, False, False, False, False, False, False, False, False, True, False, True, True, False
        , False, False, False, True ]
  , testCase "isConstantNotClockReset" $
      assertResults (isConstantNotClockReset testTypeEnv)
        [ False, False, False, False, False, False, False, False, False, True, False, True, True, False
        , False, False, False, True ]
  , testCase "isWorkFreeClockOrResetOrEnable" $
      assertResults (isWorkFreeClockOrResetOrEnable testTypeEnv)
        (replicate 16 Nothing <> [Just True, Nothing])
  , testCase "isWorkFreeIsh" $
      assertResults (isWorkFreeIsh testTypeEnv)
        [ False, False, False, False, False, False, False, False, False, True, False, False, True, False
        , False, False, True, True ]
  ]
