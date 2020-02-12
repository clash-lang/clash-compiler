{-|
Copyright  :  (C) 2019, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.Util.Interpolate where

import qualified Clash.Util.Interpolate as I

import Test.Tasty
import Test.Tasty.HUnit

test1, test2, test3, test4, test5, test6, test7, test8 :: String
test1 = [I.i| Simple |]
test2 = [I.i|
  Single line
|]
test3 = [I.i|

  Surrounded by newlines

|]
test4 = [I.i|
  One
  Two
  Three
|]
test5 = [I.i|
  One
    Two
  Three
|]
test6 = [I.i|
  #{test5}
|]

test7 = [I.i|
  The big test:

  #{test5}
|]

test8 = [I.i|
  looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong word
|]

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Core.Util.Interpolation"
    [ testCase "test1" $ "Simple" @=? test1
    , testCase "test2" $ "Single line" @=? test2
    , testCase "test3" $ "Surrounded by newlines" @=? test3
    , testCase "test4" $ "One Two Three" @=? test4
    , testCase "test5" $ "One\n  Two\nThree" @=? test5
    , testCase "test6" $ test5 @=? test6
    , testCase "test7" $ test7 @=? ("The big test:\n\n" ++ test5)
    , testCase "test8" $ test8 @=? "looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong \nword"
    ]
