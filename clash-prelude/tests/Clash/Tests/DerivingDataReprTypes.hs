{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Tests.DerivingDataReprTypes where

import Clash.Annotations.BitRepresentation.Deriving
import Clash.Sized.Unsigned

type SmallInt = Unsigned 2

data Train
  = Passenger
      -- Number of wagons:
      SmallInt
  | Freight
      -- Number of wagons:
      SmallInt
      -- Max weight:
      SmallInt
  | Maintenance
  | Toy

data RGB
  = R
  | G
  | B

deriveDefaultAnnotation [t|RGB|]
deriveBitPack [t|RGB|]

data Headphones
  = InEar
      {_wireless :: Bool}
  | OverEar
      {_impedance :: SmallInt}

data EarCup = SmallInt :<>: SmallInt
