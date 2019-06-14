{-# LANGUAGE DataKinds #-}

-- Issue #359: https://github.com/clash-lang/clash-compiler/issues/359

module NewType where

import           Clash.Prelude

newtype Circuit a b c d = Circuit ((a,b) -> (c, d))

topEntity
  :: Clock System
  -> Reset System
  -> Circuit (Signal System Int) () () (Signal System Int)
topEntity c r = withClockReset c r $ Circuit $ \(i,()) -> ((), register 0 i)
