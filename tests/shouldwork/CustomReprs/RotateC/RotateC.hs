{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module RotateC where

import Clash.Prelude.Testbench
import Clash.Prelude

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


type Top =
     SystemClockResetEnable
  => Signal System MaybeColor
  -> Signal System Color

topEntity :: Top
topEntity = fmap f
  where
    f cM =
      case cM of
        JustC c  -> rotateColor c
        NothingC -> Red
{-# NOINLINE topEntity #-}

-- Testbench:
tb :: Top -> Signal System Bool
tb top = done'
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
    done = expectedOutput (top testInput)
    done' =
      withClockResetEnable
        (tbSystemClockGen (not <$> done'))
        systemResetGen
        enableGen
        done
{-# INLINE tb #-}

testBench :: Signal System Bool
testBench = tb topEntity
{-# NOINLINE testBench #-}
