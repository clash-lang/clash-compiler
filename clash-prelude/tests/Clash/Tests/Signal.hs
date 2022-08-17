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

import           Clash.Signal.Internal          (dynamicClockGen, sample)

import           Control.Exception              (evaluate)
import           Data.List                      (isInfixOf)
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Language.Haskell.Interpreter   as Hint
import           Language.Haskell.Interpreter   (OptionVal((:=)))

createDomain vSystem{vName="H11", vPeriod=hzToPeriod 11}
createDomain vSystem{vName="H77", vPeriod=hzToPeriod 77}

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
              ((a,_):_) = sample @(Signal System) s
            in
              evaluate a >> pure ()
        ]
    , testGroup "unsafeSynchronizer"
      [ testCase "case_dynamicStaticEq" case_dynamicStaticEq
      , testCase "case_dynamicHasEffect" case_dynamicHasEffect
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
    dclk11 = dynamicClockGen @H11 (pure (hzToPeriod 11))
    dclk77 = dynamicClockGen @H77 (pure (hzToPeriod 77))

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

    clk11 = clockGen @H11
    clk77 = clockGen @H77
    dclk11 = dynamicClockGen @H11 (pure (hzToPeriod 11))
    dclk77lying = dynamicClockGen @H77 (pure (hzToPeriod 78))

    counter :: forall dom. Signal dom Int
    counter = fromList [0..]

  assertBool "clk11+clk77 /= dclk11+dclk77lying" $
       (sampleMagicN (E.unsafeSynchronizer clk11 clk77 counter))
    /= (sampleMagicN (E.unsafeSynchronizer dclk11 dclk77lying counter))
