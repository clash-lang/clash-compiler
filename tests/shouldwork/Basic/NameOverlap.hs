module NameOverlap where

import Clash.Prelude

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "nameoverlap"
    , t_inputs =
          [ PortName "CLK_50MHZ"
          , PortName "RESET"
          ]
    , t_output = PortName "LED"
    }) #-}
topEntity
    :: Clock System Source
    -> Reset System Asynchronous
    -> Signal System Bit
topEntity = exposeClockReset board
  where
    board = boolToBit <$> led
    led = register True $ complement <$> led
