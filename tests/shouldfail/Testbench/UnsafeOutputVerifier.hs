module UnsafeOutputVerifier where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock XilinxSystem
  -> Reset XilinxSystem
  -> Signal IntelSystem Int   -- Note different domain
  -> Signal XilinxSystem Bool
topEntity clk rst = outputVerifier clk rst (0 :> Nil)
