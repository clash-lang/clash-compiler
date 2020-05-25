{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module RotateC where

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
        JustC c  -> rotateColor c
        NothingC -> Red
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

    expectedOutput a = outputVerifier' (Red
                                   :> Green
                                   :> Blue
                                   :> Red
                                   :> Nil) a

    done :: _ => _
    done = expectedOutput (topEntity testInput)
    done' =
      withClockResetEnable
        (tbSystemClockGen (not <$> done'))
        systemResetGen
        enableGen
        done
