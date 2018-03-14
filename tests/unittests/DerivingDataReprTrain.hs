{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module DerivingDataReprTrain where

import Clash.Sized.Unsigned

type SmallInt = Unsigned 2

data Train
  = Passenger
      SmallInt
      -- ^ Number of wagons
  | Freight
      SmallInt
      -- ^ Number of wagons
      SmallInt
      -- ^ Max weight
  | Maintenance
  | Toy
