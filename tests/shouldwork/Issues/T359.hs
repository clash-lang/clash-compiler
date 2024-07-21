{-# LANGUAGE DataKinds #-}

-- Issue #359: https://github.com/clash-lang/clash-compiler/issues/359

module T359 where

import Clash.Prelude

newtype Circuit a b c d = Circuit ((a,b) -> (c, d))

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Circuit (Signal System Int) () () (Signal System Int)
topEntity clk rst ena =
  withClockResetEnable clk rst ena $
    Circuit $ \(i,()) -> ((), register 0 i)
