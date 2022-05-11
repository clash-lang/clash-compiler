module Reg where

import Control.Monad (when)
import Clash.Explicit.Prelude
import Clash.Explicit.SimIO

tbMachine :: Reg Int -> Int -> SimIO Int
tbMachine st _ = do
  now <- readReg st

  if now == 0 then finish 0 else display "Step" >> writeReg st (now - 1)

  return (now - 1)

tbInit :: SimIO (Reg Int)
tbInit = reg 16

topEntity :: Signal System Int
topEntity = regOut
  where
    clk = systemClockGen
    rst = resetGen
    ena = enableGen

    regOut = register clk rst ena 16 regIn
    regIn  = mealyIO clk tbMachine tbInit regOut
