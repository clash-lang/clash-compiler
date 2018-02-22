{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Rotate where

import Clash.Prelude.Testbench
import Clash.Prelude
import GHC.Generics
import Data.Maybe

-- Test data structures:
data Color
  = Red
  | Green
  | Blue
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
  :: SystemClockReset
  => Signal System (Maybe Color)
  -> Signal System Color
topEntity = fmap f
  where
    f cM =
      case cM of
        Just c  -> rotateColor c
        Nothing -> Red
{-# NOINLINE topEntity #-}

-- Testbench:
testBench :: Signal System Bool
testBench = done'
  where
    testInput = stimuliGenerator $ Nothing
                               :> (Just Red)
                               :> (Just Green)
                               :> (Just Blue)
                               :> Nil

    expectedOutput = outputVerifier $ Red
                                   :> Green
                                   :> Blue
                                   :> Red
                                   :> Nil

    done  = expectedOutput (topEntity testInput)
    done' = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
