{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module RotateCNested where

import Clash.Prelude.Testbench
import Clash.Prelude
import GHC.Generics
import Clash.Annotations.BitRepresentation
import Data.Maybe

-- Test data structures:
data Color
  = Red
  | Green
  | Blue
    deriving (Eq, Show, Generic, ShowX)

data MaybeColor
  = NothingC
  | JustC Color
    deriving (Eq, Show, Generic, ShowX)

{-# ANN module (
  DataReprAnn
    $(liftQ [t| Color |])
    2
    [ ConstrRepr
        'Red
        0b11
        0b00
        []
    , ConstrRepr
        'Blue
        0b11
        0b10
        []
    , ConstrRepr
        'Green
        0b11
        0b01
        []
    ]) #-}

{-# ANN module (
  DataReprAnn
    $(liftQ [t| MaybeColor |])
    2
    [ ConstrRepr
        'NothingC
        0b11 -- Mask
        0b11 -- Value
        []
    , ConstrRepr
        'JustC
        0b00   -- Mask
        0b00   -- Value
        [0b11] -- Masks
    ]) #-}

-- Test functions:
rotateColor
  :: Color
  -> Color
rotateColor c =
  case c of
    Red   -> Green
    Green -> Blue
    Blue  -> Red

topEntity
  :: SystemClockResetEnable
  => Signal System (Maybe MaybeColor)
  -> Signal System Color
topEntity = fmap f
  where
    f cM =
      case cM of
        Just (JustC c) -> rotateColor c
        Just NothingC  -> Blue
        Nothing        -> Red
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

-- Testbench:
testBench :: Signal System Bool
testBench = done'
  where
    testInput :: _ => _
    testInput = stimuliGenerator $ Nothing
                               :> Just (NothingC)
                               :> Just (JustC Red)
                               :> Just (JustC Green)
                               :> Just (JustC Blue)
                               :> Nil

    expectedOutput :: _ => _
    expectedOutput = outputVerifier' $ Red
                                   :> Blue
                                   :> Green
                                   :> Blue
                                   :> Red
                                   :> Nil

    done :: _ => _
    done  = expectedOutput (topEntity testInput)
    done' =
      withClockResetEnable
        (tbSystemClockGen (not <$> done'))
        systemResetGen
        enableGen
        done
