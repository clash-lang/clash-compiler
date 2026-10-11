{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.EtaExpand"
-}

{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.Normalize.Transformations.EtaExpand (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Term (..))
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (mkVarSet)
import Clash.Normalize.Transformations.EtaExpand
  (etaExpandSynWorker, etaExpansionTL)
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState)

import Test.Clash.Rewrite
  ( assertAlphaEq, globalId, inScopeOf, intFunTy, localId, parseToTermQQ
  , runSingleTransformation, showPprU )

-- | Run 'etaExpansionTL' on a closed term
runEtaTL :: Term -> IO Term
runEtaTL = runSingleTransformation def st def etaExpansionTL
 where
  st :: RewriteState NormalizeState
  st = def

-- | 'etaExpansionTL' moves the lambdas in the body of a let-expression out of
-- that let-expression:
--
-- > let i = x in \y -> e  ==>  \y -> let i = x in e
--
-- It used to do so without renaming @y@. If @y@ has the same unique as @i@,
-- the let-binder captures the references to @y@ in @e@.
etaTLLetCapturesLambda :: Assertion
etaTLLetCapturesLambda = do
  res <- runEtaTL [parseToTermQQ|
    let
      i_1 :: Int
      i_1 = 3
    in
      \(i_1 :: Int) -> i_1
  |]
  assertAlphaEq
    [parseToTermQQ|
      \(y_2 :: Int) ->
        let
          i_1 :: Int
          i_1 = 3
        in
          y_2
    |]
    res

-- | Like 'etaTLLetCapturesLambda', but now the moved lambda captures a free
-- variable of the right-hand side of the let-binding: if @y@ is free in @x@,
-- then @\\y -> let i = x in e@ binds it.
etaTLLambdaCapturesLet :: Assertion
etaTLLambdaCapturesLet = do
  res <- runEtaTL [parseToTermQQ|
    \(y_1 :: Int) ->
      let
        i_2 :: Int
        i_2 = y_1
      in
        \(y_1 :: Int) -> i_2
  |]
  assertAlphaEq
    [parseToTermQQ|
      \(y_1 :: Int) (y_3 :: Int) ->
        let
          i_2 :: Int
          i_2 = y_1
        in
          i_2
    |]
    res

-- * etaExpandSyn

-- | A global function @f :: Int -> Int@
fGlobal :: Id
fGlobal = globalId User "f" 10 (intFunTy 1)

-- | A local variable with the same unique as 'fGlobal'
fLocal :: Id
fLocal = localId User "f" 10 (intFunTy 1)

-- | 'etaExpandSyn' eta-expands references to top entities. It shares
-- 'topEntities' with 'Clash.Normalize.Transformations.Specialize.specialize',
-- and also looked up whether a variable is a top entity by its unique alone,
-- so it eta-expanded a local that shares its unique with a top entity.
etaExpandSynLocal :: Assertion
etaExpandSynLocal = do
  expanded <- etaExpandSyn1 fGlobal
  case expanded of
    Lam _ (App (Var f) (Var _)) | f == fGlobal -> pure ()
    _ -> assertFailure ("Expected an eta-expanded top entity, but got:\n" <> showPprU expanded)
  etaExpandSyn1 fLocal >>= assertAlphaEq (Var fLocal)
 where
  -- Run 'etaExpandSyn' on a reference to a variable, with 'fGlobal' as the
  -- only top entity
  etaExpandSyn1 v =
    runSingleTransformation env st (inScopeOf [v]) etaExpandSynWorker (Var v)
  env = def{_topEntities = mkVarSet [fGlobal]}
  st = def :: RewriteState NormalizeState

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.EtaExpand"
    [ testCase "etaExpansionTL: let-binder does not capture lambda"
        etaTLLetCapturesLambda
    , testCase "etaExpansionTL: lambda does not capture let-binding"
        etaTLLambdaCapturesLet
    , testCase "etaExpandSyn does not expand a local that shares a unique with a top entity"
        etaExpandSynLocal
    ]
