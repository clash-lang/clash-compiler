module T3354 where

import Clash.Prelude

-- | Compiled with @-dynamic@ by the testsuite: Clash used to additionally set
-- @-dynamic-too@, which made GHC warn that it got ignored. See
-- https://github.com/clash-lang/clash-compiler/issues/3354.
topEntity :: Signal System Bit -> Signal System Bit
topEntity = fmap complement
