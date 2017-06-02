{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module CLaSH.Xilinx.ClockGen where

import CLaSH.Signal.Internal
import CLaSH.Promoted.Symbol
import Unsafe.Coerce

clockWizard
  :: SSymbol name
  -> Clock  pllIn 'Source
  -> Reset  pllIn 'Asynchronous
  -> (Clock pllOut 'Source, Signal pllOut Bool)
clockWizard _ clk (Async rst) = (unsafeCoerce (clockGate clk rst), unsafeCoerce rst)
{-# NOINLINE clockWizard #-}

clockWizardDifferential
  :: SSymbol name
  -> Clock pllIn 'Source
  -> Clock pllIn 'Source
  -> Reset pllIn 'Asynchronous
  -> (Clock pllOut 'Source, Signal pllOut Bool)
clockWizardDifferential _name clkP _clkN (Async rst) =
  (unsafeCoerce (clockGate clkP rst), unsafeCoerce rst)
{-# NOINLINE clockWizardDifferential #-}
