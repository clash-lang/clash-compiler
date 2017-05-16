{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module CLaSH.Intel.ClockGen where

import CLaSH.Explicit.Signal
import CLaSH.Promoted.Symbol
import Unsafe.Coerce

altpll
  :: SSymbol name
  -> Clock  pllIn 'Source
  -> Reset  pllIn 'Asynchronous
  -> (Clock pllOut 'Source, Signal pllOut Bool)
altpll _ clk (Async rst) = (unsafeCoerce (clockGate clk rst), unsafeCoerce rst)
{-# NOINLINE altpll #-}
