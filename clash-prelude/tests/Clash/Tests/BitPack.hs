{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Tests.BitPack where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.HUnit.Extra (expectXException)

import Clash.Class.BitPack
import Clash.Sized.BitVector
import Clash.Sized.Index
import Clash.Sized.RTree
import Clash.Sized.Signed
import Clash.Sized.Unsigned
import Clash.Sized.Vector
import Clash.XException

import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat)

data Unit    = Unit                        deriving (Generic, BitPack, Eq, Show)
data Wrapper = Wrapper Int                 deriving (Generic, BitPack, Eq, Show)
data Sum     = SumTypeA | SumTypeB         deriving (Generic, BitPack, Eq, Show)
data BigSum  = BS1 | BS2 | BS3 | BS4 | BS5 deriving (Generic, BitPack, Eq, Show)
data Product = Product Int Int             deriving (Generic, BitPack, Eq, Show)
data SP      = S Int Int | P Int           deriving (Generic, BitPack, Eq, Show)
data Rec1    = Rec1 { a :: Int }           deriving (Generic, BitPack, Eq, Show)
data Rec2    = Rec2 { b :: Int, c :: Int } deriving (Generic, BitPack, Eq, Show)
newtype WrappedIndex = WrappedIndex (Index 3) deriving (Generic, BitPack, Eq, Show)

rtt :: (Eq a, Show a, BitPack a) => a -> Assertion
rtt u = unpack (pack u) @?= u

assertMaybeUnpackTable
  :: forall a n
   . (BitPack a, Eq a, Show a, KnownNat n)
  => Vec n (BitVector (BitSize a))
  -> Vec n (Maybe a)
  -> Assertion
assertMaybeUnpackTable inputs expected =
  fmap (maybeUnpack @a) inputs @?= expected

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

maybeUnpackIndex0 :: Assertion
maybeUnpackIndex0 =
  maybeUnpack @(Index 0) (0 :: BitVector (BitSize (Index 0))) @?= Nothing

maybeUnpackIndex1 :: Assertion
maybeUnpackIndex1 =
  maybeUnpack @(Index 1) (0 :: BitVector (BitSize (Index 1))) @?= Just 0

maybeUnpackIndex3 :: Assertion
maybeUnpackIndex3 =
  maybeUnpack @(Index 3) (maxBound :: BitVector (BitSize (Index 3))) @?= Nothing

maybeUnpackIndex4 :: Assertion
maybeUnpackIndex4 =
  maybeUnpack @(Index 4) (maxBound :: BitVector (BitSize (Index 4))) @?= Just 3

maybeUnpackCharMaxBound :: Assertion
maybeUnpackCharMaxBound =
  maybeUnpack @Char (0x10ffff :: BitVector (BitSize Char)) @?= Just maxBound

maybeUnpackCharOutOfRange :: Assertion
maybeUnpackCharOutOfRange =
  maybeUnpack @Char (0x110000 :: BitVector (BitSize Char)) @?= Nothing

maybeUnpackVec0 :: Assertion
maybeUnpackVec0 =
  maybeUnpack @(Vec 0 (Index 3)) 0 @?= Just Nil

maybeUnpackVec2 :: Assertion
maybeUnpackVec2 = do
  maybeUnpack @(Vec 2 (Index 3)) 0b00_10 @?= Just (0 :> 2 :> Nil)
  maybeUnpack @(Vec 2 (Index 3)) 0b11_00 @?= Nothing

maybeUnpackPair :: Assertion
maybeUnpackPair = do
  let pair = (1 :: Index 3, 2 :: Unsigned 2)
  maybeUnpack (pack pair) @?= Just pair
  maybeUnpack @(Index 3, Unsigned 2) 0b11_01 @?= Nothing

maybeUnpackIndex4All :: Assertion
maybeUnpackIndex4All =
  assertMaybeUnpackTable @(Index 4)
    ( 0b00
    :> 0b01
    :> 0b10
    :> 0b11
    :> Nil )
    ( Just 0
    :> Just 1
    :> Just 2
    :> Just 3
    :> Nil )

maybeUnpackIndex5All :: Assertion
maybeUnpackIndex5All =
  assertMaybeUnpackTable @(Index 5)
    ( 0b000
    :> 0b001
    :> 0b010
    :> 0b011
    :> 0b100
    :> 0b101
    :> 0b110
    :> 0b111
    :> Nil )
    ( Just 0
    :> Just 1
    :> Just 2
    :> Just 3
    :> Just 4
    :> Nothing
    :> Nothing
    :> Nothing
    :> Nil )

maybeUnpackBigSumAll :: Assertion
maybeUnpackBigSumAll =
  assertMaybeUnpackTable @BigSum
    ( 0b000
    :> 0b001
    :> 0b010
    :> 0b011
    :> 0b100
    :> 0b101
    :> 0b110
    :> 0b111
    :> Nil )
    ( Just BS1
    :> Just BS2
    :> Just BS3
    :> Just BS4
    :> Just BS5
    :> Nothing
    :> Nothing
    :> Nothing
    :> Nil )

maybeUnpackWrappedIndex :: Assertion
maybeUnpackWrappedIndex =
  maybeUnpack @WrappedIndex (maxBound :: BitVector (BitSize WrappedIndex)) @?= Nothing

maybeUnpackWrappedIndexAll :: Assertion
maybeUnpackWrappedIndexAll =
  assertMaybeUnpackTable @WrappedIndex
    ( 0b00
    :> 0b01
    :> 0b10
    :> 0b11
    :> Nil )
    ( Just (WrappedIndex 0)
    :> Just (WrappedIndex 1)
    :> Just (WrappedIndex 2)
    :> Nothing
    :> Nil )

maybeUnpackTuple4All :: Assertion
maybeUnpackTuple4All =
  assertMaybeUnpackTable @(Index 3, Index 3, Index 3, Index 3)
    ( 0b00_01_10_00
    :> 0b11_01_10_00
    :> 0b00_11_10_00
    :> 0b00_01_11_00
    :> 0b00_01_10_11
    :> Nil )
    ( Just (0, 1, 2, 0)
    :> Nothing
    :> Nothing
    :> Nothing
    :> Nothing
    :> Nil )

unpackBigSumInvalid :: Assertion
unpackBigSumInvalid = expectXException (unpack @BigSum 0b101)

unpackWrappedIndexInvalid :: Assertion
unpackWrappedIndexInvalid = expectXException (unpack @WrappedIndex 0b11)

packTuple4Layout :: Assertion
packTuple4Layout =
  pack ((0, 1, 2, 0) :: (Index 3, Index 3, Index 3, Index 3))
    @?= ( 0b00_01_10_00
          :: BitVector (BitSize (Index 3, Index 3, Index 3, Index 3))
        )

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
    , testGroup
        "maybeUnpack"
        [ testCase "Index 0 rejects its only bit pattern" maybeUnpackIndex0
        , testCase "Index 1 accepts its only bit pattern" maybeUnpackIndex1
        , testCase "Index 3 rejects out-of-range bit patterns" maybeUnpackIndex3
        , testCase "Index 4 accepts its full state space" maybeUnpackIndex4
        , testCase "Char accepts its maximum code point" maybeUnpackCharMaxBound
        , testCase "Char rejects out-of-range code points" maybeUnpackCharOutOfRange
        , testCase "Empty Vec accepts its only bit pattern" maybeUnpackVec0
        , testCase "Vec propagates element validity and payloads" maybeUnpackVec2
        , testCase "Pairs propagate field validity and payloads" maybeUnpackPair
        , testCase "Index 4 accepts every bit pattern in its state space" maybeUnpackIndex4All
        , testCase "Index 5 only accepts in-range bit patterns" maybeUnpackIndex5All
        , testCase "Generic sums reject unused constructor tags" maybeUnpackBigSumAll
        , testCase "Generic propagation returns Nothing for invalid Index fields" maybeUnpackWrappedIndex
        , testCase "Generic propagation only rejects wrapped invalid Index fields" maybeUnpackWrappedIndexAll
        , testCase "Tuples reject an invalid field at every position" maybeUnpackTuple4All
        , testCase "Balanced tuple packing preserves field order" packTuple4Layout
        ]
    , testGroup
        "unpack"
        [ testCase "Generic sums are undefined for unused constructor tags" unpackBigSumInvalid
        , testCase "Generic types are undefined for invalid fields" unpackWrappedIndexInvalid
        ]
    , testCase "undefSpineVec" undefSpineVec
    , testCase "undefSpineRTree" undefSpineRTree
    ]
