{-# LANGUAGE DataKinds #-}

-- Issue #359: https://github.com/clash-lang/clash-compiler/issues/359

module T359 where

import Clash.Prelude
import Clash.Class.HasDomain.HasSingleDomain

newtype Circuit a b c d = Circuit ((a,b) -> (c, d))

-- XXX: Broken instance, but I'm just trying to get this test to work again
type instance TryDomain t (Circuit a b c d) = (Merge t a d)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Circuit (Signal System Int) () () (Signal System Int)
topEntity clk rst ena =
  withClockResetEnable clk rst ena $
    Circuit $ \(i,()) -> ((), register 0 i)
