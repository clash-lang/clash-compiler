{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}

module DerivingDataRepr where


import Test.Tasty
import Test.Tasty.HUnit
import Prelude ((=<<), ($))
import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Annotations.BitRepresentation.Internal
import DerivingDataReprTrain (Train)

oneHotOverlapRepr :: DataReprAnn
oneHotOverlapRepr = $( (simpleDerivator OneHot Overlap) =<< [t| Train |] )

oneHotOverlapRepr' :: DataRepr'
oneHotOverlapRepr' =
  DataRepr'
    (ConstTy' "DerivingDataReprTrain.Train")
    8
    [ ConstrRepr' "DerivingDataReprTrain.Passegner"   0 16  16  [0b1100]
    , ConstrRepr' "DerivingDataReprTrain.Freight"     1 32  32  [0b1100, 0b0011]
    , ConstrRepr' "DerivingDataReprTrain.Maintenance" 2 64  64  []
    , ConstrRepr' "DerivingDataReprTrain.Toy"         3 128 128 []
    ]

oneHotWideRepr :: DataReprAnn
oneHotWideRepr = $( (simpleDerivator OneHot Wide) =<< [t| Train |] )

oneHotWideRepr' :: DataRepr'
oneHotWideRepr' =
  DataRepr'
    (ConstTy' "DerivingDataReprTrain.Train")
    10
    [ ConstrRepr' "DerivingDataReprTrain.Passegner"   0 64  64  [0b110000]
    , ConstrRepr' "DerivingDataReprTrain.Freight"     1 128 128 [0b001100, 0b000011]
    , ConstrRepr' "DerivingDataReprTrain.Maintenance" 2 256 256 []
    , ConstrRepr' "DerivingDataReprTrain.Toy"         3 512 512 []
    ]

countOverlapRepr :: DataReprAnn
countOverlapRepr = $( (simpleDerivator Count Overlap) =<< [t| Train |] )

countOverlapRepr' :: DataRepr'
countOverlapRepr' =
  DataRepr'
    (ConstTy' "DerivingDataReprTrain.Train")
    6
    [ ConstrRepr' "DerivingDataReprTrain.Passegner"   0 0b110000 0b000000 [0b001100]
    , ConstrRepr' "DerivingDataReprTrain.Freight"     1 0b110000 0b010000 [0b001100,0b000011]
    , ConstrRepr' "DerivingDataReprTrain.Maintenance" 2 0b110000 0b100000 []
    , ConstrRepr' "DerivingDataReprTrain.Toy"         3 0b110000 0b110000 []
    ]

countWideRepr :: DataReprAnn
countWideRepr = $( (simpleDerivator Count Wide) =<< [t| Train |] )

countWideRepr' :: DataRepr'
countWideRepr' =
  DataRepr'
    (ConstTy' "DerivingDataReprTrain.Train")
    8
    [ ConstrRepr' "DerivingDataReprTrain.Passegner"   0 0b11000000 0b00000000 [0b110000]
    , ConstrRepr' "DerivingDataReprTrain.Freight"     1 0b11000000 0b01000000 [0b001100,0b000011]
    , ConstrRepr' "DerivingDataReprTrain.Maintenance" 2 0b11000000 0b10000000 []
    , ConstrRepr' "DerivingDataReprTrain.Toy"         3 0b11000000 0b11000000 []
    ]

tests :: [TestTree]
tests =
  [ testCase "OneHotOverlap" $ dataReprAnnToDataRepr' oneHotOverlapRepr @?= oneHotOverlapRepr'
  , testCase "OneHotWide"    $ dataReprAnnToDataRepr' oneHotWideRepr    @?= oneHotWideRepr'
  , testCase "CountOverlap"  $ dataReprAnnToDataRepr' countOverlapRepr  @?= countOverlapRepr'
  , testCase "CountWide"     $ dataReprAnnToDataRepr' countWideRepr     @?= countWideRepr'
  ]
