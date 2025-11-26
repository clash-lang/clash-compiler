{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Tests.BitPack where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Class.BitPack
import Clash.Sized.RTree
import Clash.Sized.Signed
import Clash.Sized.Unsigned
import Clash.Sized.Vector
import Clash.XException

import GHC.Generics (Generic)

data Unit    = Unit                        deriving (Generic, BitPack, Eq, Show)
data Wrapper = Wrapper Int                 deriving (Generic, BitPack, Eq, Show)
data Sum     = SumTypeA | SumTypeB         deriving (Generic, BitPack, Eq, Show)
data BigSum  = BS1 | BS2 | BS3 | BS4 | BS5 deriving (Generic, BitPack, Eq, Show)
data Product = Product Int Int             deriving (Generic, BitPack, Eq, Show)
data SP      = S Int Int | P Int           deriving (Generic, BitPack, Eq, Show)
data Rec1    = Rec1 { a :: Int }           deriving (Generic, BitPack, Eq, Show)
data Rec2    = Rec2 { b :: Int, c :: Int } deriving (Generic, BitPack, Eq, Show)

rtt :: (Eq a, Show a, BitPack a) => a -> Assertion
rtt u = unpack (pack u) @?= u

{-
@pack@ will still output defined elements even if the spine is not fully defined.
-}
undefSpineVec :: Assertion
undefSpineVec = showX (pack v) @?= "0b0001_...._...."
 where
  v :: Vec 3 (Unsigned 4)
  v = 1 :> Clash.XException.undefined

{-
@pack@ will still output defined elements even if the spine is not fully defined.
-}
undefSpineRTree :: Assertion
undefSpineRTree = showX (pack t) @?= "0b...._...._0101_...."
 where
  t :: RTree 2 (Unsigned 4)
  t = BR Clash.XException.undefined (BR (LR 5) Clash.XException.undefined)

tests :: TestTree
tests =
  testGroup
    "BitPack"
    [ testGroup
        "Generic"
        [ testCase "Unit" (rtt Unit)
        , testCase "Wrapper" (rtt (Wrapper 3))
        , testCase "SumTypeA" (rtt SumTypeA)
        , testCase "SumTypeB" (rtt SumTypeB)
        , testCase "BigSum1" (rtt BS1)
        , testCase "BigSum2" (rtt BS2)
        , testCase "BigSum3" (rtt BS3)
        , testCase "BigSum4" (rtt BS4)
        , testCase "BigSum5" (rtt BS5)
        , testCase "Product" (rtt (Product 3 5))
        , testCase "SP1" (rtt (S 3 5))
        , testCase "SP2" (rtt (P 10))
        , testCase "Rec1" (rtt (Rec1 10))
        , testCase "Rec2" (rtt (Rec2 10 30))
        ]
    , testCase "Vec" (rtt ((1 :: Signed 6) :> 2 :> (-5) :> 4 :> Nil))
    , testCase "undefSpineVec" undefSpineVec
    , testCase "undefSpineRTree" undefSpineRTree
    ]
