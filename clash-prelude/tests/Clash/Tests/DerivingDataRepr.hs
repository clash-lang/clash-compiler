{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Tests.DerivingDataRepr where

import Test.Tasty
import Test.Tasty.HUnit
import Prelude ((=<<), ($))
import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Tests.DerivingDataReprTypes (Train(..), RGB(..), Headphones(..), EarCup(..))
import Data.Maybe (Maybe(..))

---------------------------------------------------------
------------ DERIVING SIMPLE REPRESENTATIONS ------------
---------------------------------------------------------
oneHotOverlapRepr :: DataReprAnn
oneHotOverlapRepr = $( (simpleDerivator OneHot OverlapL) =<< [t| Train |] )

oneHotOverlapRepr' :: DataReprAnn
oneHotOverlapRepr' =
  DataReprAnn
    $(liftQ [t| Train |])
    8
    [ ConstrRepr 'Passenger   16  16  [0b1100]
    , ConstrRepr 'Freight     32  32  [0b1100, 0b0011]
    , ConstrRepr 'Maintenance 64  64  []
    , ConstrRepr 'Toy         128 128 []
    ]

oneHotOverlapReprRec :: DataReprAnn
oneHotOverlapReprRec = $( (simpleDerivator OneHot OverlapL) =<< [t| Headphones |] )

oneHotOverlapReprRec' :: DataReprAnn
oneHotOverlapReprRec' =
  DataReprAnn
    $(liftQ [t| Headphones |])
    4
    [ ConstrRepr 'InEar   4  4  [0b10]
    , ConstrRepr 'OverEar 8  8  [0b11]
    ]

oneHotOverlapReprInfix :: DataReprAnn
oneHotOverlapReprInfix = $( (simpleDerivator OneHot OverlapL) =<< [t| EarCup |] )

oneHotOverlapReprInfix' :: DataReprAnn
oneHotOverlapReprInfix' =
  DataReprAnn
    $(liftQ [t| EarCup |])
    5
    [ ConstrRepr '(:<>:) 16  16  [0b1100,0b0011] ]


oneHotWideRepr :: DataReprAnn
oneHotWideRepr = $( (simpleDerivator OneHot Wide) =<< [t| Train |] )

oneHotWideRepr' :: DataReprAnn
oneHotWideRepr' =
  DataReprAnn
    $(liftQ [t| Train |])
    10
    [ ConstrRepr 'Passenger   64  64  [0b110000]
    , ConstrRepr 'Freight     128 128 [0b001100, 0b000011]
    , ConstrRepr 'Maintenance 256 256 []
    , ConstrRepr 'Toy         512 512 []
    ]

countOverlapRepr :: DataReprAnn
countOverlapRepr = $( (simpleDerivator Binary OverlapL) =<< [t| Train |] )

countOverlapRepr' :: DataReprAnn
countOverlapRepr' =
  DataReprAnn
    $(liftQ [t| Train |])
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
    $(liftQ [t| Train |])
    8
    [ ConstrRepr 'Passenger   0b11000000 0b00000000 [0b110000]
    , ConstrRepr 'Freight     0b11000000 0b01000000 [0b001100,0b000011]
    , ConstrRepr 'Maintenance 0b11000000 0b10000000 []
    , ConstrRepr 'Toy         0b11000000 0b11000000 []
    ]

------------------------------------------------
------------ PACKED REPRESENTATIONS ------------
------------------------------------------------

packedRepr :: DataReprAnn
packedRepr = $( packedDerivator =<< [t| Train |] )

packedRepr' :: DataReprAnn
packedRepr' =
  DataReprAnn
    $(liftQ [t| Train |])
    5
    [ ConstrRepr 'Freight     0b10000 0 [12,3]
    , ConstrRepr 'Passenger   3       1 [12]
    , ConstrRepr 'Toy         3       2 []
    , ConstrRepr 'Maintenance 3       3 []
    ]

------------------------------------------------------
------------ PACKED MAYBE REPRESENTATIONS ------------
------------------------------------------------------

packedMaybeRGB :: DataReprAnn
packedMaybeRGB = $( packedMaybeDerivator $(defaultDerivator =<< [t| Maybe RGB |]) =<< [t| Maybe RGB |] )

packedMaybeRGB' :: DataReprAnn
packedMaybeRGB' =
  DataReprAnn
    $(liftQ [t| Maybe RGB |])
    2
    [ ConstrRepr 'Nothing 0b11 0b11 []
    , ConstrRepr 'Just    0b00 0b00 [0b11]
    ]

-- MAIN
tests :: TestTree
tests = testGroup "DerivingDataRepr"
  [ testCase "OneHotOverlap"      $ oneHotOverlapRepr      @?= oneHotOverlapRepr'
  , testCase "OneHotOverlapRec"   $ oneHotOverlapReprRec   @?= oneHotOverlapReprRec'
  , testCase "OneHotOverlapInfix" $ oneHotOverlapReprInfix @?= oneHotOverlapReprInfix'
  , testCase "OneHotWide"         $ oneHotWideRepr         @?= oneHotWideRepr'
  , testCase "BinaryOverlap"      $ countOverlapRepr       @?= countOverlapRepr'
  , testCase "BinaryWide"         $ countWideRepr          @?= countWideRepr'
  , testCase "Packed"             $ packedRepr             @?= packedRepr'
  , testCase "PackedMaybe"        $ packedMaybeRGB         @?= packedMaybeRGB'
  ]
