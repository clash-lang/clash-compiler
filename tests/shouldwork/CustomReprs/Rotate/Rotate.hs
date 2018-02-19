{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Rotate where

import Clash.Prelude.Testbench
import Clash.Prelude
import Data.Maybe

-- Test data structures:
data Color
  = Red
  | Green
  | Blue
    deriving (Eq, Show)

-- Test functions:
rotateColor
  :: Color
  -> Color
rotateColor c =
  case c of
    Red   -> Green
    Green -> Blue
    Blue  -> Red

colorToInt
  :: Color
  -> Int
colorToInt Red   = 33
colorToInt Green = 34
colorToInt Blue  = 35

topEntity
  :: SystemClockReset
  => Signal System (Maybe Color)
  -> Signal System Int
topEntity = fmap (colorToInt . f)
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

    expectedOutput = outputVerifier $ 33 -- Red
                                   :> 34 -- Green
                                   :> 35 -- Blue
                                   :> 33 -- Red
                                   :> Nil

    done  = expectedOutput (topEntity testInput)
    done' = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
