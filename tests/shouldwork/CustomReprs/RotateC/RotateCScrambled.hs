{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module RotateCScrambled where

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
    deriving (Eq, Show{-, Generic, ShowX-})

data MaybeColor
  -- We purposely define the order of these constructors in the opposite order
  -- of the bit repr file.
  = JustC Color
  | NothingC
    deriving (Eq, Show{-, Generic, ShowX-})

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
  => Signal System MaybeColor
  -> Signal System Int
topEntity = fmap (colorToInt . f)
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
    testInput = stimuliGenerator $ NothingC
                               :> (JustC Red)
                               :> (JustC Green)
                               :> (JustC Blue)
                               :> Nil

    expectedOutput = outputVerifier $ 35 -- Red
                                   :> 34 -- Green
                                   :> 35 -- Blue
                                   :> 33 -- Red
                                   :> Nil

    done  = expectedOutput (topEntity testInput)
    done' = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
