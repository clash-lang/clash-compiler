{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
                  2022, Google Inc.
                  2023, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Tests.Signal where

import qualified Prelude as P
import           Prelude hiding (undefined)

import qualified Clash.Explicit.Prelude         as E
import           Clash.Prelude                  hiding (sample)

import           Clash.Signal.Internal
  (Femtoseconds(..), dynamicClockGen, sample, head#)

import           Control.Exception              (evaluate)
import           Test.Tasty
import           Test.Tasty.HUnit

createDomain vSystem{vName="H11", vPeriod=hzToPeriod 11}
createDomain vSystem{vName="H77", vPeriod=hzToPeriod 77}

createDomain vSystem{vName="F1", vPeriod=20}
createDomain vSystem{vName="F6", vPeriod=250}

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Signal"
    [ testGroup
        "Implicit"
        [ -- See: https://github.com/clash-lang/clash-compiler/pull/655
          let rst0 = fromList [True, True, False, False, True, True]
              rst1 = unsafeFromActiveHigh rst0
              reg  = register 'a' (pure 'b')
              sig = withReset @System rst1 reg
          in  testCase "withReset behavior" (sampleN @System 6 sig @?= "aaabaa")

        , testCase "T1521" $
            let
              f (_, b) = (b, b)
              s = f <$> liftA2 (,) (fst <$> s) (pure 'a')
              a = fst (head# s)
            in
              evaluate a >> pure ()
        ]
    , testGroup "unsafeSynchronizer"
      [ testCase "case_dynamicStaticEq" case_dynamicStaticEq
      , testCase "case_dynamicHasEffect" case_dynamicHasEffect
      , testCase "case_changingDynamicClocks" case_changingDynamicClocks
      , testCase "case_F1_F6" case_F1_F6
      , testCase "case_F6_F1" case_F6_F1
      ]
    ]

-- | Asserts that static clocks behave the same as dynamic clocks with a static
-- period signal passed into it.
case_dynamicStaticEq :: Assertion
case_dynamicStaticEq = do
  let
    sampleMagicN = P.take 500 . sample

    clk11 = clockGen @H11
    clk77 = clockGen @H77

    -- We construct periods in a roundabout way (i.e., using 'hzToPeriod' instead
    -- of using 'hzToFs'), to prevent rounding errors between periods of the
    -- static clocks and the periods of the dynamic clocks.
    fs11 = Femtoseconds (1000 * hzToPeriod 11)
    fs77 = Femtoseconds (1000 * hzToPeriod 77)

    dclk11 = dynamicClockGen @H11 (pure fs11)
    dclk77 = dynamicClockGen @H77 (pure fs77)

    counter :: forall dom. Signal dom Int
    counter = fromList [0..]

  assertEqual
    "clk11+clk77 == dclk11+dclk77"
    (sampleMagicN (E.unsafeSynchronizer clk11 clk77 counter))
    (sampleMagicN (E.unsafeSynchronizer dclk11 dclk77 counter))

  assertEqual
    "clk11+dclk77 == dclk11+clk77"
    (sampleMagicN (E.unsafeSynchronizer clk11 dclk77 counter))
    (sampleMagicN (E.unsafeSynchronizer dclk11 clk77 counter))

-- | Asserts that "lying" about a clock's frequency has effect.
case_dynamicHasEffect :: Assertion
case_dynamicHasEffect = do
  let
    sampleMagicN = P.take 500 . sample

    -- We construct periods in a roundabout way (i.e., using 'hzToPeriod' instead
    -- of using 'hzToFs'), to prevent rounding errors between periods of the
    -- static clocks and the periods of the dynamic clocks.
    fs11 = Femtoseconds (1000 * hzToPeriod 11)
    fs77lying = Femtoseconds (1000 * hzToPeriod 78)

    clk11 = clockGen @H11
    clk77 = clockGen @H77
    dclk11 = dynamicClockGen @H11 (pure fs11)
    dclk77lying = dynamicClockGen @H77 (pure fs77lying)

    counter :: forall dom. Signal dom Int
    counter = fromList [0..]

  assertBool "clk11+clk77 /= dclk11+dclk77lying" $
       (sampleMagicN (E.unsafeSynchronizer clk11 clk77 counter))
    /= (sampleMagicN (E.unsafeSynchronizer dclk11 dclk77lying counter))

-- | Regression test
case_changingDynamicClocks :: Assertion
case_changingDynamicClocks = do
  let
    dclk11 = dynamicClockGen @H11 $ fromList $ cycle $ fmap Femtoseconds
      [10, 20, 30, 40, 50, 60, 70, 80, 90]
    dclk77 = dynamicClockGen @H77 $ fromList $ cycle $ fmap Femtoseconds
      [90, 80, 70, 60, 50, 40, 30, 20, 10]

    counter = fromList [(0 :: Int)..]
    actual = P.take 70 (sample (E.unsafeSynchronizer dclk11 dclk77 counter))

  assertEqual "unsafeSynchronizer produced hardcoded results" actual
    [ 0, 4, 6, 7, 8, 8, 9, 9, 9, 9, 13, 15, 16, 17, 17, 18, 18, 18, 18, 22, 24
    , 25, 26, 26, 27, 27, 27, 27, 31, 33, 34, 35, 35, 36, 36, 36, 36, 40, 42
    , 43, 44, 44, 45, 45, 45, 45, 49, 51, 52, 53, 53, 54, 54, 54, 54, 58, 60
    , 61, 62, 62, 63, 63, 63, 63, 67, 69, 70, 71, 71, 72
    ]

-- | Regression test
case_F1_F6 :: Assertion
case_F1_F6 = do
  let
    clk1 = clockGen @F1
    clk6 = clockGen @F6

    counter = fromList [(0 :: Int)..]

    actual = P.take 70 (sample (E.unsafeSynchronizer clk1 clk6 counter))

  assertEqual "unsafeSynchronizer produced hardcoded results" actual
    [ 0, 13, 25, 38, 50, 63, 75, 88, 100, 113, 125, 138, 150, 163, 175, 188, 200
    , 213, 225, 238, 250, 263, 275, 288, 300, 313, 325, 338, 350, 363, 375, 388
    , 400, 413, 425, 438, 450, 463, 475, 488, 500, 513, 525, 538, 550, 563, 575
    , 588, 600, 613, 625, 638, 650, 663, 675, 688, 700, 713, 725, 738, 750, 763
    , 775, 788, 800, 813, 825, 838, 850, 863
    ]

-- | Regression test
case_F6_F1 :: Assertion
case_F6_F1 = do
  let
    clk1 = clockGen @F1
    clk6 = clockGen @F6

    counter = fromList [(0 :: Int)..]

    actual = P.take 70 (sample (E.unsafeSynchronizer clk6 clk1 counter))

  assertEqual "unsafeSynchronizer produced hardcoded results" actual
    [ 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
    , 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4
    , 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6
    ]
