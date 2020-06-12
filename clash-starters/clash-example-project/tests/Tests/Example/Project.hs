module Tests.Example.Project where

import Prelude
import Test.Tasty
import qualified Test.Tasty.QuickCheck as T

import Example.Project (plus)

main :: IO ()
main = defaultMain tests

aEqualsA :: Int -> Bool
aEqualsA a = a == a


tests :: TestTree
tests = testGroup "Example"
  [ testGroup "Project"
    [ T.testProperty "plus a b == plus b a" (\a b -> plus a b == plus b a)
    ]
  ]

