{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.Specialize"
-}

{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.Normalize.Transformations.Specialize (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Pretty (showPpr)
import Clash.Core.Term (Term (..))
import Clash.Core.VarEnv (mkInScopeSet, mkVarSet)
import Clash.Normalize.Transformations.Specialize (appPropWorker)

import Test.Clash.Rewrite
  (parseToTermQQ, parseToTerm, runSingleTransformation)

-- | @(\f -> f) (\k x -> k) x y@ reduces to the free variable @x@. The first
-- argument binds @x_2@, which is also free in the second argument. 'appProp'
-- substitutes work-free arguments using 'unsafeSubstTm', which does not avoid
-- capture: after substituting the first argument for @f_1@, that argument is
-- the head of the application, and substituting @x_2@ for @k_4@ puts it under
-- the binder @x_2@.
appPropCapture :: IO Term
appPropCapture = do
  Var x <- pure (parseToTerm "x_2 :: Int")
  Var y <- pure (parseToTerm "y_5 :: Int")
  let is = mkInScopeSet (mkVarSet [x, y])
  runSingleTransformation def def is appPropWorker [parseToTermQQ|
    (\(f_1 :: Int) -> f_1) (\(k_4 :: Int) (x_2 :: Int) -> k_4) (x_2 :: Int) (y_5 :: Int)
  |]

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.Specialize"
    [ testCase "appProp does not capture free variables" $ do
        actual <- appPropCapture
        assertBool ("Expected the free variable x, but got: " <> showPpr actual)
          (actual == parseToTerm "x_2 :: Int")
    ]
