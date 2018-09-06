{-|
Copyright  :  (C) 2017-2018, Google Inc
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

PLL and other clock-related components for Intel (Altera) FPGAs
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ExplicitForAll    #-}

module Clash.Intel.ClockGen
  ( altpll
  , alteraPll
  ) where

import Clash.Clocks           (clocks, Clocks)
import Clash.Promoted.Symbol  (SSymbol)
import Clash.Signal.Internal
  (Signal, ClockKind(..), Clock, Reset, ResetKind(..) )


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
-- type Dom100MHz = Dom \"A\" 10000
--
-- -- outputs a clock running at 100 MHz
-- altpll @@Dom100MHz (SSymbol @@"altpll50to100") clk50 rst
-- @
altpll
  :: forall pllOut pllIn name
   . SSymbol name
  -- ^ Name of the component, must correspond to the name entered in the QSys
  -- dialog.
  --
  -- For example, when you entered \"altPLL50\", instantiate as follows:
  --
  -- > SSymbol @ "altPLL50"
  -> Clock  pllIn 'Source
  -- ^ Free running clock (i.e. a clock pin connected to a crystal)
  -> Reset  pllIn 'Asynchronous
  -- ^ Reset for the PLL
  -> (Clock pllOut 'Source, Signal pllOut Bool)
  -- ^ (Stable PLL clock, PLL lock)
altpll _ = clocks
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
-- You must use type applications to specify the output clock domain, e.g.:
--
-- @
-- type Dom100MHz = Dom \"A\" 10000
--
-- -- outputs a clock running at 100 MHz
-- alteraPll @@Dom100MHz (SSymbol @@"alteraPll50to100") clk50 rst
-- @
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
  -- > SSymbol @ "alteraPLL50"
  -> Clock pllIn 'Source
  -- ^ Free running clock (i.e. a clock pin connected to a crystal)
  -> Reset pllIn 'Asynchronous
  -- ^ Reset for the PLL
  -> t
alteraPll _ = clocks
{-# NOINLINE alteraPll #-}
