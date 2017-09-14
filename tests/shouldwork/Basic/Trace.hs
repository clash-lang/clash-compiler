module Trace where

import Clash.Prelude
import Debug.Trace

topEntity :: Unsigned 32 -> Unsigned 32
topEntity y = trace "some trace" y + traceShowId y
