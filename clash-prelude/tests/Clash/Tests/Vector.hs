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
case_showXVector = "<1,2,X" @=? showX (1 :> i 2 :> errorX "def")

case_showX2DVector :: Assertion
case_showX2DVector = "<<1,X,<3,5>>" @=? showX ((1 :> errorX "def") :> (3 :> i 5 :> Nil) :> Nil)

tests :: TestTree
tests = testGroup "All"
  [ $(testGroupGenerator)
  ]

-- Run with:
--
--    ./repld p:tests -T Clash.Tests.Vector.main
--
-- Add -W if you want to run tests in spite of warnings
--
main :: IO ()
main = defaultMain tests
