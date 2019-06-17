{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE MagicHash      #-}

module Clash.Tests.Undefined where

import Test.Tasty
import Test.Tasty.HUnit

import GHC.Generics (Generic)
import Clash.XException (Undefined(rnfX), errorX)

data Void                                  deriving (Generic, Undefined)
data Unit    = Unit                        deriving (Generic, Undefined)
data Wrapper = Wrapper Int                 deriving (Generic, Undefined)
data Sum     = SumTypeA | SumTypeB         deriving (Generic, Undefined)
data BigSum  = BS1 | BS2 | BS3 | BS4 | BS5 deriving (Generic, Undefined)
data Product = Product Int Int             deriving (Generic, Undefined)
data SP      = S Int Int | P Int           deriving (Generic, Undefined)
data Rec0    = Rec0 {  }                   deriving (Generic, Undefined)
data Rec1    = Rec1 { a :: Int }           deriving (Generic, Undefined)
data Rec2    = Rec2 { b :: Int, c :: Int } deriving (Generic, Undefined)

undef :: a
undef = errorX "!"
{-# NOINLINE undef #-}

tests :: TestTree
tests =
  testGroup
    "Undefined"
    [ testGroup
        "Generic"
        [ testCase "Unit"     $ rnfX (undef :: Unit)                  @?= ()
        , testCase "Wrapper1" $ rnfX (undef :: Wrapper)               @?= ()
        , testCase "Wrapper2" $ rnfX (Wrapper undef)                  @?= ()
        , testCase "Sum"      $ rnfX (undef :: Sum)                   @?= ()
        , testCase "BigSum"   $ rnfX (undef :: BigSum)                @?= ()
        , testCase "Product1" $ rnfX (undef :: Product)               @?= ()
        , testCase "Product2" $ rnfX (Product undef undef :: Product) @?= ()
        , testCase "Product3" $ rnfX (Product 3 undef :: Product)     @?= ()
        , testCase "Product4" $ rnfX (Product undef 5 :: Product)     @?= ()
        , testCase "SP1"      $ rnfX (undef :: SP)                    @?= ()
        , testCase "SP2"      $ rnfX (S undef undef :: SP)            @?= ()
        , testCase "SP3"      $ rnfX (S 3 undef :: SP)                @?= ()
        , testCase "SP3"      $ rnfX (S undef 5 :: SP)                @?= ()
        , testCase "SP4"      $ rnfX (P undef :: SP)                  @?= ()
        , testCase "Rec0"     $ rnfX (undef :: Rec0)                  @?= ()
        , testCase "Rec1_1"   $ rnfX (undef :: Rec1)                  @?= ()
        , testCase "Rec1_2"   $ rnfX (Rec1 undef)                     @?= ()
        , testCase "Rec2_1"   $ rnfX (undef :: Rec2)                  @?= ()
        , testCase "Rec2_2"   $ rnfX (Rec2 3 undef)                   @?= ()
        , testCase "Rec2_3"   $ rnfX (Rec2 undef 5)                   @?= ()
--          Test case broken on 8.2.2:
--        , testCase "Void"     $ rnfX (undef :: Void)                  @?= ()
        ]
    , testGroup
        "Manual"
        [ testCase "List1"     $ rnfX (undef :: [Int])                @?= ()
        , testCase "List2"     $ rnfX ([undef] :: [Int])              @?= ()
        , testCase "Maybe1"    $ rnfX (undef :: Maybe Int)            @?= ()
        , testCase "Maybe2"    $ rnfX (Just undef :: Maybe Int)       @?= ()
        , testCase "Either1"   $ rnfX (undef :: Either Int Int)       @?= ()
        , testCase "Either2"   $ rnfX (Left undef :: Either Int Int)  @?= ()
        , testCase "Either3"   $ rnfX (Right undef :: Either Int Int) @?= ()
        ]
    ]

