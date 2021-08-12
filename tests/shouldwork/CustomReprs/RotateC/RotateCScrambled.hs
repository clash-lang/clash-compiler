{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module RotateCScrambled
  ( topEntity
  , testBench
  ) where

import Clash.Prelude.Testbench
import Clash.Prelude
import Prelude hiding (not)
import Data.Maybe
import GHC.Generics
import Clash.Annotations.BitRepresentation

-- Test data structures:
data Color
  = Red
  | Green
  | Blue
    deriving (Eq, Show, Generic, ShowX)

data MaybeColor
  -- We purposely define the order of these constructors in the opposite order
  -- of the bit repr file.
  = JustC Color
  | NothingC
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
  => Signal System MaybeColor
  -> Signal System Color
topEntity = fmap f
  where
    f cM =
      case cM of
        NothingC -> Blue
        JustC c  -> rotateColor c
{-# NOINLINE topEntity #-}

-- Testbench:
testBench :: Signal System Bool
testBench = done'
  where
    testInput :: _ => _
    testInput = stimuliGenerator $ NothingC
                               :> (JustC Red)
                               :> (JustC Green)
                               :> (JustC Blue)
                               :> Nil

    expectedOutput a = outputVerifier' (Blue
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
