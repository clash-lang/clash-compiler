{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Tests.DerivingDataReprTypes where

import Clash.Sized.Unsigned
import Clash.Annotations.BitRepresentation.Deriving

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

deriveDefaultAnnotation [t| RGB |]
deriveBitPack [t| RGB |]
