module CoerceClockN where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (ClockN)
import Data.Coerce (coerce)

topEntity :: ClockN System -> ClockN XilinxSystem
topEntity = coerce
