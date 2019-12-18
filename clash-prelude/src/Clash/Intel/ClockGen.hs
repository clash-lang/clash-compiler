{-|
Copyright  :  (C) 2017-2018, Google Inc
                  2019     , Myrtle Software
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

PLL and other clock-related components for Intel (Altera) FPGAs
-}

{-# LANGUAGE FlexibleContexts  #-}

module Clash.Intel.ClockGen
  ( altpll
  , alteraPll
  ) where

import Clash.Clocks           (clocks, Clocks)
import Clash.Promoted.Symbol  (SSymbol)
import Clash.Signal.Internal
  (Signal, Clock, Reset)


-- | A clock source that corresponds to the Intel/Quartus \"ALTPLL\" component
-- with settings to provide a stable 'Clock' from a single free-running input
--
-- Only works when configured with:
--
-- * 1 reference clock
-- * 1 output clock
-- * a reset port
-- * a locked port
--
-- You must use type applications to specify the output clock domain, e.g.:
--
-- @
-- -- outputs a clock running at 100 MHz
-- altpll @@"50MHzDom" @@"100MHzDom" (SSymbol @@"altpll50to100") clk50 rst
-- @
altpll
  :: forall domOut domIn name
   . SSymbol name
  -- ^ Name of the component, must correspond to the name entered in the QSys
  -- dialog.
  --
  -- For example, when you entered \"altPLL50\", instantiate as follows:
  --
  -- > SSymbol @"altPLL50"
  -> Clock domIn
  -- ^ Free running clock (i.e. a clock pin connected to a crystal)
  -> Reset domIn
  -- ^ Reset for the PLL
  -> (Clock domOut, Signal domOut Bool)
  -- ^ (Stable PLL clock, PLL lock)
altpll !_ = clocks
{-# NOINLINE altpll #-}

-- | A clock source that corresponds to the Intel/Quartus \"Altera PLL\"
-- component with settings to provide a stable 'Clock' from a single
-- free-running input
--
-- Only works when configured with:
--
-- * 1 reference clock
-- * 1-16 output clocks
-- * a reset input port
-- * a locked output port
--
-- The number of output clocks depend this function's inferred result type. An
-- instance with a single and double output clock can be instantiated using:
--
-- @
-- (outClk, pllLocked) = alteraPll clk rst
-- @
--
-- and
--
-- @
-- (outClk1, outClk2, pllLocked) = alteraPll clk rst
-- @
--
-- respectively.
alteraPll
  :: Clocks t
  => SSymbol name
  -- ^ Name of the component, must correspond to the name entered in the QSys
  -- dialog.
  --
  -- For example, when you entered \"alteraPLL50\", instantiate as follows:
  --
  -- > SSymbol @"alteraPLL50"
  -> Clock domIn
  -- ^ Free running clock (i.e. a clock pin connected to a crystal)
  -> Reset domIn
  -- ^ Reset for the PLL
  -> t
alteraPll !_ = clocks
{-# NOINLINE alteraPll #-}
