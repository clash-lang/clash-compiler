{-|
Copyright  :  (C) 2020, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations where

import Data.Default (def)
import Data.Maybe (fromMaybe)

import Clash.Core.Name (NameSort (User))
import Clash.Normalize.Transformations (inlineBndrsCleanup, splitCastWork)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteState (..))
import Clash.Util.Supply (freshId, newSupply)
import Clash.Core.VarEnv
  (emptyInScopeSet, mkInScopeSet, mkVarSet, mkVarEnv, emptyVarEnv)
import Clash.Core.FreeVars (countFreeOccurances)
import Clash.Core.Term

import Test.Tasty
import Test.Tasty.HUnit

import Test.Clash.Rewrite
  ( assertAlphaEq, globalId, intFunTy, intId, intLit, intTy
  , parseToTermQQ, parseToTerm, runSingleTransformation )

t1337a :: Term
t1337a = fromMaybe (error "failed to build term") $ do
  Letrec binds result <- pure $
    [parseToTermQQ|
      let
        -- Types don't mean anything for this example
        result_1, a_2, b_3, c_4 :: Int

        result_1 = a_2

        a_2 = b_3
        b_3 = c_4
        c_4 = a_2 b_3
      in
        result_1
    |]

  (keep0:inlines) <- pure (map (\(v,e) -> (v,((v,e),countFreeOccurances e))) binds)
  let is = mkInScopeSet (mkVarSet (map fst binds))

  let keep1 = inlineBndrsCleanup is (mkVarEnv inlines) emptyVarEnv [snd keep0]

  return (Letrec keep1 result)

t1337a_result :: Term
t1337a_result = [parseToTermQQ|
  let
    result_1, b_3 :: Int
    result_1 = b_3
    b_3 = b_3 b_3
  in
    result_1
|]

t1337b :: Term
t1337b = fromMaybe (error "failed to build term") $ do

  Letrec binds result <- pure $
    [parseToTermQQ|
      let
        -- Types don't mean anything for this example
        result_1, a_2, b_3, c_4, d_5 :: Int

        result_1 = a_2

        a_2 = b_3
        b_3 = c_4
        c_4 = d_5
        d_5 = a_2 b_3 c_4
      in
        result_1
    |]

  (keep0:inlines) <- pure (map (\(v,e) -> (v,((v,e),countFreeOccurances e))) binds)
  let is = mkInScopeSet (mkVarSet (map fst binds))

  let keep1 = inlineBndrsCleanup is (mkVarEnv inlines) emptyVarEnv [snd keep0]

  return (Letrec keep1 result)

t1337b_result :: Term
t1337b_result = [parseToTermQQ|
  let
    result_1, c_4 :: Int
    result_1 = c_4
    c_4 = c_4 c_4 c_4
  in
    result_1
|]

t1337c :: Term
t1337c = fromMaybe (error "failed to build term") $ do
  Letrec binds result <- pure $
    [parseToTermQQ|
      let
        result_1, a_2, b_3, c_4 :: Int

        result_1 = a_2

        a_2 = b_3
        b_3 = c_4
        c_4 = a_2 b_3 (freevar_5 :: Int)
      in
        result_1
    |]

  (keep0:inlines) <- pure (map (\(v,e) -> (v,((v,e),countFreeOccurances e))) binds)
  Var fv <- pure (parseToTerm "freevar_5 :: Int")
  let is = mkInScopeSet (mkVarSet (fv : map fst binds))

  let keep1 = inlineBndrsCleanup is (mkVarEnv inlines) emptyVarEnv [snd keep0]

  return (Letrec keep1 result)

t1337c_result :: Term
t1337c_result = [parseToTermQQ|
  let
    result_1, b_3 :: Int
    result_1 = b_3
    b_3 = b_3 b_3 (freevar_5 :: Int)
  in
    result_1
|]

splitCastWorkR :: NormRewrite
splitCastWorkR = $(asRewriteQ splitCastWork)

-- | @letrec y = 3; x = cast (g y) in x@, where @y@ has the unique the supply
-- hands out first. 'splitCastWork' must give the new binder for @g y@ a unique
-- that differs from those of the let's binders:
--
-- @
-- letrec y = 3; x' = g y; x = cast x' in x
-- @
--
-- It used to make the new binder fresh with respect to the let's context only,
-- so it could get the unique of @y@.
splitCastWorkFresh :: Assertion
splitCastWorkFresh = do
  supply <- newSupply
  let
    -- The supply can hand out any unique first, so the other binders take
    -- the uniques after it, rather than fixed ones
    (uy, supply1) = freshId supply
    (ux, supply2) = freshId supply1
    (ux', _) = freshId supply2
    y = intId User "y" uy
    x = intId User "x" ux
    x' = intId User "x'" ux'
    term =
      Letrec [(y, intLit 3), (x, Cast (App g (Var y)) intTy intTy)] (Var x)
    expected =
      Letrec
        [(y, intLit 3), (x', App g (Var y)), (x, Cast (Var x') intTy intTy)]
        (Var x)
  result <-
    runSingleTransformation def def{_uniqSupply = supply} emptyInScopeSet
      splitCastWorkR term
  assertAlphaEq expected result
 where
  g = Var (globalId User "g" 2 (intFunTy 1))

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Core.Util.Interpolation"
    [ testCase "T1337a" $ t1337a_result @=? t1337a
    , testCase "T1337b" $ t1337b_result @=? t1337b
    , testCase "T1337c" $ t1337c_result @=? t1337c
    , testCase "splitCastWork binders are fresh w.r.t. the let's binders"
        splitCastWorkFresh
    ]
