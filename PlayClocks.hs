module PlayClocks where

import Clash.Clocks
import Clash.Intel.ClockGen
import Clash.Xilinx.ClockGen
import Clash.Explicit.Prelude

createDomain vSystem{vName="Dom1", vPeriod=10000}
createDomain vSystem{vName="Dom2", vPeriod=20000}
createDomain vSystem{vName="Dom3", vPeriod=30000}


myPll ::
  ( KnownDomain System
  , KnownDomain Dom1
  ) =>
  Clock System ->
  Clock System ->
  Reset System ->
  (Clock Dom1, Reset Dom1)
myPll clkInN clkInP rstIn =
  clocksSynchronizedReset (clockWizardDifferential (SSymbol @"foo") clkInN) clkInP rstIn
{-# ANN myPll (defSyn "myPll") #-}

myPllGen ::
  ( KnownDomain domIn
  , KnownDomain dom1
  ) =>
  Clock domIn ->
  Clock domIn ->
  Reset domIn ->
  (Clock dom1, Reset dom1)
myPllGen clkInN clkInP rstIn =
  clocksSynchronizedReset (clockWizardDifferential (SSymbol @"foo") clkInN) clkInP rstIn

myPll3 ::
  Clock System ->
  Reset System ->
  ( Clock Dom1
  , Reset Dom1
  , Clock Dom2
  , Reset Dom2
  , Clock Dom3
  , Reset Dom3)
myPll3 clkIn rstIn =
  clocksSynchronizedReset (alteraPll (SSymbol @"foo")) clkIn rstIn
{-# ANN myPll3 (defSyn "myPll3") #-}
