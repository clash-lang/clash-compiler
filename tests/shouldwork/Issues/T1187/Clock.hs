module T1187.Clock
    ( HzToPeriod
    , Milliseconds
    , ClockDivider
    ) where

import Clash.Prelude

type HzToPeriod (rate :: Nat) = (Seconds 1 + rate - 1) `Div` rate

type Seconds (s :: Nat) = Milliseconds (1000 * s)
type Milliseconds (ms :: Nat) = Microseconds (1000 * ms)
type Microseconds (us :: Nat) = Nanoseconds (1000 * us)
type Nanoseconds (ns :: Nat) = 1000 * ns

type ClockDivider dom ps = ps `Div` DomainPeriod dom
