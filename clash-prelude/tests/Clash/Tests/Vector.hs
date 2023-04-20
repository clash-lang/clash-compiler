{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Vector where

import           Clash.Sized.Vector (Vec((:>), Nil))
import           Clash.XException

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH

i :: Int -> Int
i = id

case_showXVector :: Assertion
case_showXVector = "1 :> 2 :> undefined" @=? showX (1 :> i 2 :> errorX "def")

case_showX2DVector :: Assertion
case_showX2DVector =
      "(1 :> undefined) :> (3 :> 5 :> Nil) :> Nil"
  @=? showX ((1 :> errorX "def") :> (3 :> i 5 :> Nil) :> Nil)

case_showX2DVectorInList :: Assertion
case_showX2DVectorInList =
      "[1 :> undefined,3 :> 5 :> Nil]"
  @=? showX [(1 :> errorX "def"), (3 :> i 5 :> Nil)]

case_showVector :: Assertion
case_showVector = "1 :> 2 :> 3 :> Nil" @=? show (1 :> 2 :> i 3 :> Nil)

case_show2DVector :: Assertion
case_show2DVector =
  "(1 :> 2 :> 3 :> Nil) :> Nil" @=? show ((1 :> 2 :> i 3 :> Nil) :> Nil)

case_showVectorInList :: Assertion
case_showVectorInList =
  "[1 :> 2 :> Nil,3 :> 4 :> Nil]" @=? show [1 :> 2 :> Nil, 3 :> i 4 :> Nil]

tests :: TestTree
tests = $(testGroupGenerator)

-- Run with:
--
--    ./repld p:tests -T Clash.Tests.Vector.main
--
-- Add -W if you want to run tests in spite of warnings
--
main :: IO ()
main = defaultMain tests
