module T2243 where

import Clash.Prelude

-- Regression test for #2243: superfluous port names in a 'Synthesize'
-- annotation used to be silently ignored. Clash should now reject them.

-- Too many names on an /argument/: the input 'PortProduct' lists three names
-- for a two-element tuple, so "arg_c" is superfluous.
{-# ANN argTop
  (Synthesize
    { t_name     = "argTop"
    , t_inputs   =
      [ PortProduct ""
        [ PortName "arg_a"
        , PortName "arg_b"
        , PortName "arg_c"
        ]
      ]
    , t_output   = PortName "arg_out"
}) #-}
argTop :: (Int, Int) -> Int
argTop (a, _) = a
{-# OPAQUE argTop #-}

-- Too many names on the /result/: the output 'PortProduct' lists three names
-- for a two-element tuple, so "res_c" is superfluous.
{-# ANN resTop
  (Synthesize
    { t_name     = "resTop"
    , t_inputs   = [ PortName "res_in" ]
    , t_output   = PortProduct ""
      [ PortName "res_a"
      , PortName "res_b"
      , PortName "res_c"
      ]
}) #-}
resTop :: Int -> (Int, Int)
resTop a = (a, a)
{-# OPAQUE resTop #-}
