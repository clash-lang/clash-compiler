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
import qualified Clash.Normalize.Scoped.Util as Scoped
import Clash.Rewrite.Scoped.Types (Frame (..), pushFrame, rootContext)

import Test.Clash.Rewrite
  ( globalId, intFunTy, intId, intLit, intTy, localId, mkBindingMap
  , nameToUnique, runRewriteTest )
import Test.Clash.Scoped

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
    [ testGroup "recursion" recursionTests
    , testGroup "inlining" inliningTests
    , testGroup "shouldReduce" shouldReduceTests
    , testGroup "callGraph" callGraphTests
    , testGroup "classifyFunction" classifyTests
    ]
