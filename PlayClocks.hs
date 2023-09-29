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

{-
myPll1 ::
  DiffClock System ->
  Reset System ->
  (Clock Dom1, Reset Dom1)
myPll1 clkIn@(DiffClock clkInP _) rstIn =
  clocksResetSynchronizer (clockWizardDifferential (SSymbol @"foo") clkIn rstIn) clkInP
{-# ANN myPll1 (defSyn "myPll1") #-}
{-# NOINLINE myPll1 #-}

myPll2 ::
  Clock System ->
  Reset System ->
  (Clock Dom1, Reset Dom1)
myPll2 clkIn rstIn =
  clocksResetSynchronizer (clockWizard (SSymbol @"foo") clkIn rstIn) clkIn
{-# ANN myPll2 (defSyn "myPll2") #-}
{-# NOINLINE myPll2 #-}
-}

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
  clocksResetSynchronizer (alteraPll (SSymbol @"foo") clkIn rstIn) clkIn
{-# ANN myPll3 (defSyn "myPll3") #-}
{-# NOINLINE myPll3 #-}

myPll4 ::
  Clock System ->
  Reset System ->
  (Clock Dom1, Reset Dom1)
myPll4 = setName @"foo" altpllSync
{-# ANN myPll4 (defSyn "myPll4") #-}
{-# NOINLINE myPll4 #-}

myPll5 ::
  Clock System ->
  Reset System ->
  ( Clock Dom1
  , Reset Dom1)
myPll5 = setName @"foo" alteraPllSync
{-# ANN myPll5 (defSyn "myPll5") #-}
{-# NOINLINE myPll5 #-}

myPll6 ::
  Clock System ->
  Reset System ->
  ( Clock Dom1
  , Reset Dom1
  , Clock Dom2
  , Reset Dom2
  , Clock Dom3
  , Reset Dom3)
myPll6 = setName @"foo" alteraPllSync
{-# ANN myPll6 (defSyn "myPll6") #-}
{-# NOINLINE myPll6 #-}

myPll7 ::
  Clock System ->
  Reset System ->
  ( Clock Dom1
  , Reset Dom1)
myPll7 = clockWizardSync
{-# ANN myPll7 (defSyn "myPll7") #-}
{-# NOINLINE myPll7 #-}

myPll8 ::
  DiffClock System ->
  Reset System ->
  ( Clock Dom1
  , Reset Dom1)
myPll8 = clockWizardDifferentialSync
{-# ANN myPll8 (defSyn "myPll8") #-}
{-# NOINLINE myPll8 #-}

myPll9 ::
  Clock System ->
  Reset System ->
  ( Clock Dom1
  , Reset Dom1)
myPll9 = setName @"foo" clockWizardSync
{-# ANN myPll9 (defSyn "myPll9") #-}
{-# NOINLINE myPll9 #-}

myPll10 ::
  DiffClock System ->
  Reset System ->
  ( Clock Dom1
  , Reset Dom1)
myPll10 = setName @"foo" clockWizardDifferentialSync
{-# ANN myPll10 (defSyn "myPll10") #-}
{-# NOINLINE myPll10 #-}

myPll11 ::
  Clock System ->
  Reset System ->
  ( Clock Dom1
  , Signal Dom1 Bool
  , Clock Dom2
  , Signal Dom2 Bool
  )
myPll11 clk rst = (clk1, lock1, clk2, lock2)
 where
  (clk1, lock1) = unsafeClockWizard clk rst
  (clk2, lock2) = unsafeClockWizard clk rst
{-# ANN myPll11 (defSyn "myPll11") #-}
{-# NOINLINE myPll11 #-}

myPll12 ::
  DiffClock System ->
  Reset System ->
  (Clock Dom1, Signal Dom1 Bool)
myPll12 = unsafeClockWizardDifferential
{-# ANN myPll12 (defSyn "myPll12") #-}
{-# NOINLINE myPll12 #-}

myPll13 ::
  Clock System ->
  Reset System ->
  (Clock Dom1, Clock Dom2, Clock Dom3, Signal System Bool)
myPll13 = unsafeClockWizard
{-# ANN myPll13 (defSyn "myPll13") #-}
{-# NOINLINE myPll13 #-}

myPll14 ::
  Clock System ->
  Reset System ->
  (Clock Dom1, Signal Dom1 Bool)
myPll14 = altpll (SSymbol @"foo")
{-# ANN myPll14 (defSyn "myPll14") #-}
{-# NOINLINE myPll14 #-}

myPll15 ::
  Clock System ->
  Reset System ->
  ( Clock Dom1
  , Clock Dom2
  , Clock Dom3
  , Signal Dom1 Bool
  )
myPll15 = alteraPll (SSymbol @"foo")
{-# ANN myPll15 (defSyn "myPll15") #-}
{-# NOINLINE myPll15 #-}
