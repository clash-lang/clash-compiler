-- Uses 'outputVerifier' with differing domains in a top entity, which
-- instantiates the non-synthesizable 'unsafeSimSynchronizer' primitive. The
-- -Werror=clash-non-synthesizable promotion should be overridden by the
-- subsequent -Wwarn=clash-non-synthesizable, so compilation should succeed
-- while still printing the warning.
module DemotedWarning where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock XilinxSystem
  -> Clock IntelSystem
  -> Reset XilinxSystem
  -> Signal IntelSystem Int   -- Note different domain
  -> Signal XilinxSystem Bool
topEntity clkX clkI rst = outputVerifier clkX clkI rst (0 :> Nil)
