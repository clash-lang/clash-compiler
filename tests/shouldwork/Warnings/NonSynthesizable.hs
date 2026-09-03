-- Instantiates the non-synthesizable 'unsafeSimSynchronizer' primitive outside
-- of a test bench, which warns with -Wclash-non-synthesizable. See the
-- "Warnings" test group in tests/Main.hs for the flag combinations this is
-- compiled with:
--
--   * -Werror=clash-non-synthesizable promotes the warning, aborting compilation
--   * a following -Wwarn=clash-non-synthesizable demotes it again
--   * -Wno-clash-non-synthesizable suppresses it altogether
module NonSynthesizable where

import Clash.Prelude
import Clash.Explicit.Testbench (unsafeSimSynchronizer)

topEntity
  :: Clock IntelSystem
  -> Clock XilinxSystem
  -> Signal IntelSystem Int
  -> Signal XilinxSystem Int   -- Note different domain
topEntity = unsafeSimSynchronizer
