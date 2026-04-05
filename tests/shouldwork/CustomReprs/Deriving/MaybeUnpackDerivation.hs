{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MaybeUnpackDerivation where

import           GHC.Generics                           (Generic)

import           Clash.Annotations.BitRepresentation    (ConstrRepr(..), DataReprAnn(..), liftQ)
import           Clash.Annotations.BitRepresentation.Deriving
  ( deriveBitPack )
import           Clash.Prelude
import           Clash.Prelude.Testbench

data Color
  = Red
  | Green
  | Blue
  deriving (Generic, NFDataX, Eq, ShowX)

-- Leave 0b11 invalid so outer custom-repr decoding has to propagate Nothing.
{-# ANN module
  (DataReprAnn
    $(liftQ [t| Color |])
    2
    [ ConstrRepr 'Red 0b11 0b00 []
    , ConstrRepr 'Green 0b11 0b01 []
    , ConstrRepr 'Blue 0b11 0b10 []
    ])
  #-}
deriveBitPack [t| Color |]

data Wrapped = Wrapped Bool Color
  deriving (Generic, NFDataX, Eq, ShowX)

{-# ANN module
  (DataReprAnn
    $(liftQ [t| Wrapped |])
    3
    [ConstrRepr 'Wrapped 0b0 0b0 [0b100, 0b011]])
  #-}
deriveBitPack [t| Wrapped |]

wasNothing :: Maybe a -> Bool
wasNothing Nothing = True
wasNothing (Just _) = False

topEntity
  :: SystemClockResetEnable
  => Signal System (BitVector 3)
  -> Signal System Bool
topEntity = fmap (wasNothing . maybeUnpack @Wrapped)
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done'
 where
  testInput :: SystemClockResetEnable => Signal System (BitVector 3)
  testInput =
    stimuliGenerator $
      0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> Nil

  expectedOutput :: SystemClockResetEnable => Signal System Bool -> Signal System Bool
  expectedOutput =
    outputVerifier' $
      False
      :> False
      :> False
      :> True
      :> False
      :> False
      :> False
      :> True
      :> Nil

  done :: SystemClockResetEnable => Signal System Bool
  done = expectedOutput (topEntity testInput)

  done' =
    withClockResetEnable
      (tbSystemClockGen (Clash.Prelude.not <$> done'))
      systemResetGen
      enableGen
      done
