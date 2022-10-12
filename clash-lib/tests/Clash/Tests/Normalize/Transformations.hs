{-|
Copyright  :  (C) 2020, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.Normalize.Transformations where

import Clash.Normalize.Transformations (inlineBndrsCleanup)
import Clash.Core.VarEnv
  (mkInScopeSet, mkVarSet, mkVarEnv, emptyVarEnv)
import Clash.Core.FreeVars (countFreeOccurances)
import Clash.Core.Term

import Test.Tasty
import Test.Tasty.HUnit

import Test.Clash.Rewrite (parseToTermQQ, parseToTerm)

t1337a :: Term
t1337a = Letrec keep1 result
 where
  (keep0:inlines)= map (\(v,e) -> (v,((v,e),countFreeOccurances e))) binds
  is = mkInScopeSet (mkVarSet (map fst binds))

  keep1 = inlineBndrsCleanup is (mkVarEnv inlines) emptyVarEnv [snd keep0]

  Letrec binds result =
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
t1337b = Letrec keep1 result
 where
  (keep0:inlines)= map (\(v,e) -> (v,((v,e),countFreeOccurances e))) binds
  is = mkInScopeSet (mkVarSet (map fst binds))

  keep1 = inlineBndrsCleanup is (mkVarEnv inlines) emptyVarEnv [snd keep0]

  Letrec binds result =
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
t1337c = Letrec keep1 result
 where
  (keep0:inlines)= map (\(v,e) -> (v,((v,e),countFreeOccurances e))) binds
  Var fv = parseToTerm "freevar_5 :: Int"
  is = mkInScopeSet (mkVarSet (fv : map fst binds))

  keep1 = inlineBndrsCleanup is (mkVarEnv inlines) emptyVarEnv [snd keep0]

  Letrec binds result =
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

t1337c_result :: Term
t1337c_result = [parseToTermQQ|
  let
    result_1, b_3 :: Int
    result_1 = b_3
    b_3 = b_3 b_3 (freevar_5 :: Int)
  in
    result_1
|]

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Core.Util.Interpolation"
    [ testCase "T1337a" $ t1337a_result @=? t1337a
    , testCase "T1337b" $ t1337b_result @=? t1337b
    , testCase "T1337c" $ t1337c_result @=? t1337c
    ]
