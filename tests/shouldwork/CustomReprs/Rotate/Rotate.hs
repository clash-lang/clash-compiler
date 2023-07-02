{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Rotate where

import Clash.Prelude.Testbench
import Clash.Prelude
import GHC.Generics
import Data.Maybe
import Clash.Annotations.BitRepresentation

-- Test data structures:
data Color
  = Red
  | Green
  | Blue
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
        'Green
        0b11
        0b01
        []
    , ConstrRepr
        'Blue
        0b11
        0b10
        []
    ]) #-}

{-# ANN module (
  DataReprAnn
    $(liftQ [t| Maybe Color |])
    2
    -- How do we represent our constructors?
    [ ConstrRepr
        'Nothing
        0b11 -- Mask
        0b11 -- Value
        []
    , ConstrRepr
        'Just
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
  => Signal System (Maybe Color)
  -> Signal System Color
topEntity = fmap f
  where
    f cM =
      case cM of
        Just c  -> rotateColor c
        Nothing -> Red
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

-- Testbench:
testBench :: Signal System Bool
testBench = done'
  where
    testInput :: _ => _
    testInput = stimuliGenerator $ Nothing
                               :> (Just Red)
                               :> (Just Green)
                               :> (Just Blue)
                               :> Nil

    expectedOutput a = outputVerifier' (Red
                                   :> Green
                                   :> Blue
                                   :> Red
                                   :> Nil) a

    done :: _ => _
    done  = expectedOutput (topEntity testInput)
    done' =
      withClockResetEnable
        (tbSystemClockGen (not <$> done'))
        systemResetGen
        enableGen
        done
