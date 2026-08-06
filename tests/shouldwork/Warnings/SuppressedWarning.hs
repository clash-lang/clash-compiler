-- Uses 'outputVerifier' with differing domains in a top entity, which
-- instantiates the non-synthesizable 'unsafeSimSynchronizer' primitive. The
-- warning is suppressed with -Wno-clash-non-synthesizable; if suppression
-- failed, the -Werror=clash-non-synthesizable promotion would abort
-- compilation and this test would fail.
module SuppressedWarning where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock XilinxSystem
  -> Clock IntelSystem
  -> Reset XilinxSystem
  -> Signal IntelSystem Int   -- Note different domain
  -> Signal XilinxSystem Bool
topEntity clkX clkI rst = outputVerifier clkX clkI rst (0 :> Nil)
