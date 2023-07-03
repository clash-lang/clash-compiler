{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module NoCPR where

import           Clash.Prelude

{-# ANN example
  (Synthesize
          { t_name   = "example"
          , t_inputs = [ PortName "a"
                       ]
          , t_output = PortProduct ""
                         [ PortName "b"
                         , PortName "c"
                         ]
          }
  )#-}

example :: Signal System (BitVector 1) -> Signal System (BitVector 1, BitVector 1)
example input = foo $ bundle (input, pure 0)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE foo #-}
foo :: Signal dom (BitVector 1, BitVector 1)
    -> Signal dom (BitVector 1, BitVector 1)
foo input = input
