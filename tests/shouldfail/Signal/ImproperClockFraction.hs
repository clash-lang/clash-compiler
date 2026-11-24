module ImproperClockFraction where

import Clash.Explicit.Prelude
import GHC.Real (Ratio((:%)))

data Fast
type instance ClockDomainOf Fast = Fast
instance KnownClockDomain Fast where
  type ClockPeriod Fast = 2
  type ClockPeriodFraction Fast = 3 :% 2
  type ClockActiveEdge Fast = 'Rising
  type ClockResetKind Fast = 'Asynchronous
  type ClockInitBehavior Fast = 'Defined
  type ClockResetPolarity Fast = 'ActiveHigh

topEntity :: Bool -> Bool
topEntity = id
