-- Uses 'outputVerifier' with differing domains in a top entity, which
-- instantiates the non-synthesizable 'unsafeSimSynchronizer' primitive. With
-- -Werror=clash-non-synthesizable this warning should abort compilation.
module PromotedWarning where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock XilinxSystem
  -> Clock IntelSystem
  -> Reset XilinxSystem
  -> Signal IntelSystem Int   -- Note different domain
  -> Signal XilinxSystem Bool
topEntity clkX clkI rst = outputVerifier clkX clkI rst (0 :> Nil)
