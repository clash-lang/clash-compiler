{-# OPTIONS_GHC -Wno-orphans #-}
module PlayClocks where

import Clash.Clocks
import Clash.Intel.ClockGen
import Clash.Xilinx.ClockGen
import Clash.Explicit.Prelude

createDomain vSystem{vName="Dom1", vPeriod=10000}
createDomain vSystem{vName="Dom2", vPeriod=20000}
createDomain vSystem{vName="Dom3", vPeriod=30000}

myPllGen ::
  ( KnownDomain domIn
  , KnownDomain dom1
  ) =>
  DiffClock domIn ->
  Reset domIn ->
  (Clock dom1, Reset dom1)
myPllGen clkIn rstIn =
  clocksSynchronizedReset (clockWizardDifferential (SSymbol @"foo")) clkIn rstIn

myPll1 ::
  DiffClock System ->
  Reset System ->
  (Clock Dom1, Reset Dom1)
myPll1 clkIn rstIn =
  clocksSynchronizedReset (clockWizardDifferential (SSymbol @"foo")) clkIn rstIn
{-# ANN myPll1 (defSyn "myPll1") #-}

myPll2 ::
  Clock System ->
  Reset System ->
  (Clock Dom1, Reset Dom1)
myPll2 clkIn rstIn =
  clocksSynchronizedReset (clockWizard (SSymbol @"foo")) clkIn rstIn
{-# ANN myPll2 (defSyn "myPll2") #-}

myPll3 ::
  Clock System ->
  Reset System ->
  ( Clock Dom1
  , Clock Dom2
  , Reset Dom2
  , Clock Dom3
  , Reset Dom3)
myPll3 clkIn rstIn = (clk1, clk2, rst2, clk3, rst3)
 where
   (clk1, _rst1 :: Reset Dom1, clk2, rst2, clk3, rst3) = clocksSynchronizedReset (alteraPll (SSymbol @"foo")) clkIn rstIn
{-# ANN myPll3 (defSyn "myPll3") #-}
