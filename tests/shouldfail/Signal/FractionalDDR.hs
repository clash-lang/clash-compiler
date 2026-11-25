module FractionalDDR where

import Clash.Explicit.DDR
import Clash.Explicit.Prelude

createClockDomain vSystem{vName="Slow", vPeriod=10}
createClockDomain vSystem{vName="Fast", vPeriod=5.5}

-- Truncating the fast period would incorrectly accept this pair.
topEntity :: Clock Slow -> Reset Slow -> Enable Slow -> Signal Fast Bool -> Signal Slow (Bool, Bool)
topEntity clk rst en = ddrIn clk rst en (False, False, False)
