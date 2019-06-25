module MultipleHidden where

import Clash.Prelude
import Clash.Explicit.Testbench

import Unsafe.Coerce (unsafeCoerce)

createDomain vSystem{vTag="DomA"}
createDomain vSystem{vTag="DomB"}

multiClockAdd
  :: ( HiddenClockResetEnable domA confA
     , HiddenClockResetEnable domB confB )
  => Signal domA (Unsigned 16)
  -> Signal domB (Unsigned 16)
  -> Signal domA (Unsigned 16)
multiClockAdd a0 b0 = a1 + unsafeSynchronizer b2
 where
  a1 = register 0 a0

  b1 = register 0 b0
  b2 = register 0 b1

topEntityI
  :: forall domA domB confA confB
   . ( HiddenClockResetEnable domA confA
     , HiddenClockResetEnable domB confB )
  => Signal domA (Unsigned 16)
  -> Signal domB (Unsigned 16)
  -> Signal domA (Unsigned 16)
topEntityI a b = multiClockAdd a b
{-# NOINLINE topEntityI #-}


topEntity
  :: Clock  DomA
  -> Reset  DomA
  -> Enable DomA

  -> Clock  DomB
  -> Reset  DomB
  -> Enable DomB

  -> Signal DomA (Unsigned 16)
  -> Signal DomB (Unsigned 16)
  -> Signal DomA (Unsigned 16)
topEntity clkA rstA enA clkB rstB enB a b =
  exposeClockResetEnable
    (exposeClockResetEnable (topEntityI @DomA @DomB) clkA rstA enA)
    clkB rstB enB
    a b
{-# NOINLINE topEntity #-}

testBench :: Signal DomA Bool
testBench = done
  where
    testInputA = stimuliGenerator clkA rstA (1 :> 2 :> 3 :> 4 :> Nil)
    testInputB = stimuliGenerator clkB rstB (5 :> 6 :> 7 :> 8 :> Nil)

    expectedOutput = outputVerifier clkA rstA (0 :> 1 :> 7 :> 9 :> 11 :> Nil)

    done = expectedOutput (topEntity clkA rstA enableGen clkB rstB enableGen testInputA testInputB)
    clkA = tbClockGen (not <$> done)
    rstA = resetGen

    clkB = clockGen
    rstB = resetGen
