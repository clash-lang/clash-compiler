module NameOverlap where

import Clash.Prelude

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "nameoverlap"
    , t_inputs =
          [ PortName "CLK_50MHZ"
          , PortName "RESET"
          , PortName "ENABLE"
          ]
    , t_output = PortName "LED"
    }) #-}
topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System Bit
topEntity = exposeClockResetEnable $ let
    board = boolToBit <$> led
    led :: Signal System Bool
    led = register True $ complement <$> led
  in board
