{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests of "Test.Clash.Core.Print"
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Clash.Core.PrintTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Clash.Core.Alpha (aeqTm)
import Test.Clash.Rewrite (TestTerm, parseTestTerm, termQQ)
import Test.Clash.Core (termIn, withClashing, withTerm, varD)
import Test.Clash.Core.Print (showTestTerm)


-- | Printing a term and parsing it gives the same term, up to alpha
-- equivalence, also with binders that clash with the scope
tests :: TestTree
tests = testGroup "Test.Clash.Core.Print"
  [ testCase (show i) (roundTrip t) | (i, t) <- zip [(1 :: Int) ..] examples ]
 where
  roundTrip t = withTerm t $ \ts e -> do
    check ts e
    withClashing ts [varD "x" "Int", varD "y" "Int"] e check

  check ts e = do
    let s = showTestTerm e
        e' = termIn ts (parseTestTerm s)
    assertBool ("printed as:\n" <> s) (aeqTm e e')

  examples :: [TestTerm]
  examples =
    [ "\\(x :: Int) (y :: Int) -> f_G1 x (g_G2 y x)"
    , "\\(a :: Type) (x :: a) -> let { !(y :: a) = x; !(x :: a) = y } in x"
    , "let { (x :: Int) = f_G1 y; (y :: Int) = g_G2 x } in x"
    , "\\(x :: Int) -> (case x of { 1 -> x; _ -> let { !(x :: Int) = z } in x }) :: Int"
    , "(case m of { Just (x :: Int) -> x; Nothing -> (-3) }) :: Int"
    , "\\(e :: E) -> (case e of { MkE (b :: Type) (y :: b) -> h_G1 @b y }) :: Int"
    , "(GHC.Num.Integer.integerMul :: Integer -> Integer -> Integer) (3 :: Integer) (4 :: Integer)"
    , "\\(x_S12 :: Int) -> let { (w_S :: Int) = x_S12; (longName_I3 :: Int) = w_S } in longName_I3"
    , "\\(k :: Type) (a :: Core.Kind k) (x :: a) -> (case x of {}) :: Int"
    , "\\(dom :: Type) (eq :: dom ~ \"System\") (f :: (dom ~ Int) -> Int) -> eq"
    , "(Prim._CO_ :: Int) ((Core.prim @\"c$sel\" :: Int -> Int) (Prim.f :: Int))"
    , "Core.cast @Int @(Maybe Int) (Core.deDup (Core.prefixName @\"n\" x_12345678))"
    , "\\(n :: Nat) (p :: Clash.Sized.Vector.Vec n Int) -> Just @(Maybe Int) (Nothing @Int)"
    , [termQQ|
        \(veryLongName_77 :: Int) -> let { !(veryLongName_77 :: Int) = veryLongName_77 } in
          \(f :: forall (a :: Type). a -> a) -> f @(Int -> Int) (\(z :: Int) -> veryLongName_77)
      |]
    ]
