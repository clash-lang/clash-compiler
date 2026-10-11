{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Capture"
-}

module Clash.Tests.Rewrite.Capture (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import Clash.Core.VarEnv (elemVarSet)
import Clash.Rewrite.Capture (trimContext)

import Test.Clash.Rewrite (intId, intLit)

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Capture"
  [ testCase "a let-frame keeps the bindings the term uses, transitively, in order" $ do
      let frames = [C.LetBody [(a, C.Var x), (b, intLit 2), (c, C.Var a)], C.LamBody x, C.LamBody z]
          (frames', keep) = trimContext frames [C.Var c]
      case frames' of
        [C.LetBody [(a', _), (c', _)], C.LamBody _, C.LamBody _] -> do
          a' @?= a
          c' @?= c
        _ -> assertFailure "expected the bindings of a and c"
      mapM_ (\v -> assertBool (show v <> " kept") (v `elemVarSet` keep)) [a, c, x, z]
      assertBool "b not kept" (not (b `elemVarSet` keep))

  , testCase "a binding of a later frame can make one of an outer frame needed" $ do
      let frames = [C.LetBody [(c, C.Var a)], C.LetBody [(a, intLit 1), (b, intLit 2)]]
          (frames', _) = trimContext frames [C.Var c]
      case frames' of
        [C.LetBody [_], C.LetBody [(a', _)]] -> a' @?= a
        _ -> assertFailure "expected the binding of a in the outer frame"

  , testCase "a let-binding frame keeps the siblings that are used" $ do
      let frames = [C.LetBinding c [a, b, c], C.LamBody x]
          (frames', _) = trimContext frames [C.Var a]
      case frames' of
        [C.LetBinding c' sibs, _] -> do
          c' @?= c
          sibs @?= [a]
        _ -> assertFailure "expected a let-binding frame"
  ]
 where
  x = intId C.User "x" 1
  a = intId C.User "a" 2
  b = intId C.User "b" 3
  c = intId C.User "c" 4
  z = intId C.User "z" 5
