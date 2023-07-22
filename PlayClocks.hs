{-# OPTIONS_GHC -Wno-orphans #-}
module PlayClocks where

import Clash.Clocks
import Clash.Intel.ClockGen
import Clash.Xilinx.ClockGen
import Clash.Explicit.Prelude
import Clash.Signal.Internal (DiffClock(..))

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
myPllGen clkIn@(DiffClock clkInP _) rstIn =
  clocksResetSynchronizer (const (clockWizardDifferential (SSymbol @"foo") clkIn)) clkInP rstIn

myPll1 ::
  DiffClock System ->
  Reset System ->
  (Clock Dom1, Reset Dom1)
myPll1 clkIn@(DiffClock clkInP _) rstIn =
  clocksResetSynchronizer (const (clockWizardDifferential (SSymbol @"foo") clkIn)) clkInP rstIn
{-# ANN myPll1 (defSyn "myPll1") #-}

myPll2 ::
  Clock System ->
  Reset System ->
  (Clock Dom1, Reset Dom1)
myPll2 clkIn rstIn =
  clocksResetSynchronizer (clockWizard (SSymbol @"foo")) clkIn rstIn
{-# ANN myPll2 (defSyn "myPll2") #-}

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
  clocksResetSynchronizer (alteraPll (SSymbol @"foo")) clkIn rstIn
{-# ANN myPll3 (defSyn "myPll3") #-}

mySyn ::
  Clock Dom1 ->
  Reset Dom1 ->
  Reset Dom1
mySyn = resetSynchronizer
{-# ANN mySyn (defSyn "mySyn") #-}

myPll4 ::
  Clock System ->
  Reset System ->
  (Clock Dom1, Reset Dom1)
myPll4 = altpllSync (SSymbol @"foo")
{-# ANN myPll4 (defSyn "myPll4") #-}

myPll5 ::
  Clock System ->
  Reset System ->
  ( Clock Dom1
  , Reset Dom1
  , Clock Dom2
  , Reset Dom2
  , Clock Dom3
  , Reset Dom3)
myPll5 = alteraPllSync (SSymbol @"foo")
{-# ANN myPll5 (defSyn "myPll5") #-}
