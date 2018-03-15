{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}

module Clash.Tests.DerivingDataRepr where

import Test.Tasty
import Test.Tasty.HUnit
import Prelude ((=<<), ($))
import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Tests.DerivingDataReprTrain (Train(..))

oneHotOverlapRepr :: DataReprAnn
oneHotOverlapRepr = $( (simpleDerivator OneHot Overlap) =<< [t| Train |] )

oneHotOverlapRepr' :: DataReprAnn
oneHotOverlapRepr' =
  DataReprAnn
    $(reprType [t| Train |])
    8
    [ ConstrRepr 'Passenger   16  16  [0b1100]
    , ConstrRepr 'Freight     32  32  [0b1100, 0b0011]
    , ConstrRepr 'Maintenance 64  64  []
    , ConstrRepr 'Toy         128 128 []
    ]

oneHotWideRepr :: DataReprAnn
oneHotWideRepr = $( (simpleDerivator OneHot Wide) =<< [t| Train |] )

oneHotWideRepr' :: DataReprAnn
oneHotWideRepr' =
  DataReprAnn
    $(reprType [t| Train |])
    10
    [ ConstrRepr 'Passenger   64  64  [0b110000]
    , ConstrRepr 'Freight     128 128 [0b001100, 0b000011]
    , ConstrRepr 'Maintenance 256 256 []
    , ConstrRepr 'Toy         512 512 []
    ]

countOverlapRepr :: DataReprAnn
countOverlapRepr = $( (simpleDerivator Binary Overlap) =<< [t| Train |] )

countOverlapRepr' :: DataReprAnn
countOverlapRepr' =
  DataReprAnn
    $(reprType [t| Train |])
    6
    [ ConstrRepr 'Passenger   0b110000 0b000000 [0b001100]
    , ConstrRepr 'Freight     0b110000 0b010000 [0b001100,0b000011]
    , ConstrRepr 'Maintenance 0b110000 0b100000 []
    , ConstrRepr 'Toy         0b110000 0b110000 []
    ]

countWideRepr :: DataReprAnn
countWideRepr = $( (simpleDerivator Binary Wide) =<< [t| Train |] )

countWideRepr' :: DataReprAnn
countWideRepr' =
  DataReprAnn
    $(reprType [t| Train |])
    8
    [ ConstrRepr 'Passenger   0b11000000 0b00000000 [0b110000]
    , ConstrRepr 'Freight     0b11000000 0b01000000 [0b001100,0b000011]
    , ConstrRepr 'Maintenance 0b11000000 0b10000000 []
    , ConstrRepr 'Toy         0b11000000 0b11000000 []
    ]

packedRepr :: DataReprAnn
packedRepr = $( packedDerivator =<< [t| Train |] )

packedRepr' :: DataReprAnn
packedRepr' =
  DataReprAnn
    $(reprType [t| Train |])
    5
    [ ConstrRepr 'Freight     0b10000 0 [12,3]
    , ConstrRepr 'Passenger   3       1 [12]
    , ConstrRepr 'Toy         3       2 []
    , ConstrRepr 'Maintenance 3       3 []
    ]

tests :: TestTree
tests = testGroup "DerivingDataRepr"
  [ testCase "OneHotOverlap" $ oneHotOverlapRepr @?= oneHotOverlapRepr'
  , testCase "OneHotWide"    $ oneHotWideRepr    @?= oneHotWideRepr'
  , testCase "BinaryOverlap" $ countOverlapRepr  @?= countOverlapRepr'
  , testCase "BinaryWide"    $ countWideRepr     @?= countWideRepr'
  , testCase "Packed"        $ packedRepr        @?= packedRepr'
  ]
