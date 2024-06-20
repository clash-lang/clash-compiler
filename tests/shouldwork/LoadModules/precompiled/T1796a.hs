{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module T1796a where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

tb :: Signal System Bool
tb = done
  where
    done = register clk rst enableGen False $ id (pure True)
    clk = tbClockGen @System (not <$> done)
    rst = resetGen @System
