{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
                  2022, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
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
  (Femtoseconds(..), dynamicClockGen, sample)

import           Control.Exception              (evaluate)
import           Data.List                      (isInfixOf)
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Language.Haskell.Interpreter   as Hint
import           Language.Haskell.Interpreter   (OptionVal((:=)))

createDomain vSystem{vName="H11", vPeriod=hzToPeriod 11}
createDomain vSystem{vName="H77", vPeriod=hzToPeriod 77}

createDomain vSystem{vName="F1", vPeriod=20}
createDomain vSystem{vName="F6", vPeriod=250}

customTypeMark :: String
customTypeMark = "You tried to apply an explicitly routed clock, reset, or enable line"

typeCheck
  :: String
  -> IO (Either Hint.InterpreterError ())
typeCheck expr =
  Hint.runInterpreter $ do
    Hint.setImports ["Clash.Prelude"]
    Hint.set [Hint.languageExtensions := [Hint.RankNTypes, Hint.TypeApplications]]
    mapM_ Hint.runStmt [test0s, test1s, test2s, test3s, test4s]
    Hint.interpret expr (Hint.as :: ())

assertCustomTypeError :: String -> String -> Assertion
assertCustomTypeError expectedErr expr = do
  result <- typeCheck expr
  case result of
    Left err ->
      if expectedErr `isInfixOf` show err then
        pure ()
      else
        assertFailure $
             "Expression failed to typecheck as expected, but did not contain "
          <> "expected type error. Instead it contained: " <> show err
    Right () ->
      assertFailure "Expression should have failed to typecheck, but succeeded."

main :: IO ()
main = defaultMain tests

test0s, test1s, test2s, test3s :: String
test0s = "let test0 = undefined :: Signal dom1 a -> Signal dom2 Int"
test1s = "let test1 = undefined :: Int -> Char -> Int"
test2s = "let test2 = undefined :: Signal System a -> Signal XilinxSystem Int"
test3s = "let test3 = () :: ()"
test4s = "let test4 = undefined :: ((Signal dom1 a, Signal dom2 a), Signal dom3 a)"
test5s = "let test5 = undefined :: ((Char, Signal dom1 a), Signal dom2 a)"

test0 :: forall dom1 dom2. Signal dom1 Int -> Signal dom2 Int
test0 = undefined

test1 :: Int -> Char -> Int
test1 = undefined

test2 :: Signal System a -> Signal XilinxSystem Int
test2 = undefined

test3 :: ()
test3 = ()

test4
  :: forall dom1 dom2 dom3 dom4 a
   . ((Signal dom1 a, Signal dom2 a), Signal dom3 a)
  -> Signal dom4 a
test4 = undefined

test5
  :: forall dom1 dom2 a b
   . ((b, Signal dom1 a), Signal dom2 a)
test5 = undefined

acte :: String -> Assertion
acte = assertCustomTypeError customTypeMark

tests :: TestTree
tests =
  testGroup
    "Signal"
    [ testGroup
        "Implicit"
        [ -- See: https://github.com/clash-lang/clash-compiler/pull/655
          let rst0 = fromList [True, True, False, False, True, True]
              rst1 = unsafeFromHighPolarity rst0
              reg  = register 'a' (pure 'b')
#ifdef CLASH_MULTIPLE_HIDDEN
              sig = withReset rst1 reg
#else
              sig = withReset @System rst1 reg
#endif
          in  testCase "withReset behavior" (sampleN @System 6 sig @?= "aaabaa")

#ifdef CLASH_MULTIPLE_HIDDEN
          -- See: https://github.com/clash-lang/clash-compiler/pull/669
        , testCase "test0nok_0" (acte "withReset resetGen test0")
        , testCase "test0nok_1" (acte "withReset (resetGen @System) test0")
        , testCase "test0nok_2" (acte "withReset @System (resetGen @System) test0")
        , testCase
            "test0nok_3"
            (acte
              (unwords
                [ "withReset", "@System", "(resetGen @System)"
                , "(test0 :: Signal System a -> Signal dom Int)"
                ]))

        , testCase "test0nok_4" (acte "withSpecificReset resetGen test0")
        , testCase "test0nok_5" (acte "withSpecificReset (resetGen @System) test0")

        , testCase "test1nok_0" (acte "withReset resetGen test1")
        , testCase "test1nok_1" (acte "withReset (resetGen @System) test1")
        , testCase "test1nok_2" (acte "withSpecificReset resetGen test1")
        , testCase "test1nok_3" (acte "withSpecificReset (resetGen @System) test1")

        , testCase "test2nok_0" (acte "withReset resetGen test2")
        , testCase "test2nok_1" (acte "withReset (resetGen @System) test2")
        , testCase "test2nok_2" (acte "withSpecificReset resetGen test2")

        , testCase "test3nok_0" (acte "withReset resetGen test3")
        , testCase "test3nok_1" (acte "withReset (resetGen @System) test3")
        , testCase "test3nok_2" (acte "withSpecificReset resetGen test3")
        , testCase "test3nok_3" (acte "withSpecificReset (resetGen @System) test3")

        , testCase "test4nok_0" (acte $
             "withSpecificReset resetGen (test4 :: "
          ++ "((Signal System a, Signal dom2 a), Signal dom3 a)"
          ++ "-> Signal dom4 a)" )

        , testCase "test4nok_1" (acte $
             "withSpecificReset resetGen (test4 :: "
          ++ "((Signal dom1 a, Signal System a), Signal dom3 a)"
          ++ "-> Signal dom4 a)" )

        , testCase "test4nok_2" (acte $
             "withReset resetGen (test4 :: "
          ++ "((Signal System a, Signal dom2 a), Signal dom3 a)"
          ++ "-> Signal dom4 a)" )

        , testCase "test4nok_3" (acte $
             "withReset resetGen (test4 :: "
          ++ "((Signal dom1 a, Signal System a), Signal dom3 a)"
          ++ "-> Signal dom4 a)" )

        , testCase "test5nok_0" (acte "withSpecificReset resetGen test5")
#endif
        , testCase "T1521" $
            let
              f (_, b) = (b, b)
              s = f <$> liftA2 (,) (fst <$> s) (pure 'a')
              a = fst (Prelude.head (sample @(Signal System) s))
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

-- Tests below should survive compilation:
test0ok_0 = withReset @System (resetGen @System) (test0 @System @System)
#ifdef CLASH_MULTIPLE_HIDDEN
test0ok_1 = withReset resetGen (test0 @System @System)
test0ok_2 = withSpecificReset (resetGen @System) (test0 @System)
test0ok_3 = withSpecificReset (resetGen @System) (test0 @_ @System)

test2ok_0 = withSpecificReset (resetGen @System) test2
test2ok_1 = withSpecificReset @System resetGen test2

test4ok_0 = withReset resetGen (test4 @System @System @System @System)
test4ok_1 = withSpecificReset (resetGen @System) (test4 @System @_ @_ @_)
test4ok_2 = withSpecificReset (resetGen @System) (test4 @_ @System @_ @_)
test4ok_3 = withSpecificReset (resetGen @System) (test4 @_ @_ @System @_)

test5ok_0 = withSpecificReset (resetGen @System) (test5 @System @_)
#endif

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
    fs11 = Femtoseconds (fromIntegral (1000 * hzToPeriod 11))
    fs77 = Femtoseconds (fromIntegral (1000 * hzToPeriod 77))

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
    fs11 = Femtoseconds (fromIntegral (1000 * hzToPeriod 11))
    fs77lying = Femtoseconds (fromIntegral (1000 * hzToPeriod 78))

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
