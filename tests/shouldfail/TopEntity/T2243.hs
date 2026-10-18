module T2243 where

import Clash.Explicit.Prelude
import qualified Clash.Prelude as Hidden

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

-- Too many names on a clock/reset product: such products are split into
-- separate arguments before netlist generation (see 'splitTopAnn'), which used
-- to silently drop the superfluous "extra" name.
{-# ANN splitTop
  (Synthesize
    { t_name     = "splitTop"
    , t_inputs   =
      [ PortProduct ""
        [ PortName "clk"
        , PortName "rst"
        , PortName "extra"
        ]
      , PortName "x"
      ]
    , t_output   = PortName "y"
}) #-}
splitTop
  :: (Clock System, Reset System)
  -> Signal System Int
  -> Signal System Int
splitTop (clk, rst) = register clk rst enableGen 0
{-# OPAQUE splitTop #-}

-- Too many names in the /inner/ product of a nested split: products are split
-- one level at a time, so the check applies at every level. "extra" is
-- superfluous.
{-# ANN nestedTop
  (Synthesize
    { t_name     = "nestedTop"
    , t_inputs   =
      [ PortProduct ""
        [ PortName "x"
        , PortProduct "cr"
          [ PortName "clk"
          , PortName "rst"
          , PortName "extra"
          ]
        ]
      ]
    , t_output   = PortName "y"
}) #-}
nestedTop
  :: (Signal System Int, (Clock System, Reset System))
  -> Signal System Int
nestedTop (x, (clk, rst)) = register clk rst enableGen 0 x
{-# OPAQUE nestedTop #-}

-- Too many names on a vector of clocks: vectors of clocks are also split into
-- separate arguments. "extra" is superfluous.
{-# ANN vecTop
  (Synthesize
    { t_name     = "vecTop"
    , t_inputs   =
      [ PortProduct ""
        [ PortName "clk0"
        , PortName "clk1"
        , PortName "extra"
        ]
      , PortName "x"
      ]
    , t_output   = PortName "y"
}) #-}
vecTop
  :: Vec 2 (Clock System)
  -> Signal System Int
  -> Signal System Int
vecTop (clk :> _ :> Nil) x = register clk resetGen enableGen 0 x
{-# OPAQUE vecTop #-}

-- Flat names for a hidden clock/reset/enable constraint: the constraint is a
-- /single/ product port, so its parts must be named with a 'PortProduct'
-- (see 'hiddenExactTop'). Listing them as separate port names used to
-- silently name the whole product "clk" and the data input "rst".
{-# ANN hiddenTop
  (Synthesize
    { t_name     = "hiddenTop"
    , t_inputs   =
      [ PortName "clk"
      , PortName "rst"
      , PortName "en"
      , PortName "x"
      ]
    , t_output   = PortName "y"
}) #-}
hiddenTop
  :: Hidden.HiddenClockResetEnable System
  => Signal System Int
  -> Signal System Int
hiddenTop = Hidden.register 0
{-# OPAQUE hiddenTop #-}

-- Too many names on a /fully void/ result: the port is not rendered in HDL,
-- but its annotation is still checked. "res_c" is superfluous.
{-# ANN voidOutTop
  (Synthesize
    { t_name     = "voidOutTop"
    , t_inputs   = [ PortName "x" ]
    , t_output   = PortProduct ""
      [ PortName "res_a"
      , PortName "res_b"
      , PortName "res_c"
      ]
}) #-}
voidOutTop :: Signal System Int -> ((), ())
voidOutTop _ = ((), ())
{-# OPAQUE voidOutTop #-}
