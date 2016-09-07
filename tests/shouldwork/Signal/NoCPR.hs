{-# LANGUAGE DataKinds     #-}

module NoCPR where

import           CLaSH.Prelude

{-# ANN example
  (defTop { t_name = "example"
          , t_inputs = [ "a"
                       ]
          , t_outputs = [ "b"
                        , "c"
                        ]
          }
  )#-}

example :: Signal (BitVector 1) -> Signal (BitVector 1, BitVector 1)
example input = foo $ bundle (input, pure 0)

{-# NOINLINE foo #-}
foo :: Signal (BitVector 1, BitVector 1)
    -> Signal (BitVector 1, BitVector 1)
foo input = input
