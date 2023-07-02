{-# LANGUAGE CPP #-}

module MultipleHidden where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import Clash.Explicit.Testbench

import Unsafe.Coerce (unsafeCoerce)

createDomain vSystem{vName="DomA"}
createDomain vSystem{vName="DomB"}

multiClockAdd
  :: ( HiddenClockResetEnable domA
     , HiddenClockResetEnable domB )
  => Signal domA (Unsigned 16)
  -> Signal domB (Unsigned 16)
  -> Signal domA (Unsigned 16)
multiClockAdd a0 b0 = a1 + unsafeSynchronizer b2
 where
  a1 = register 0 a0

  b1 = register 0 b0
  b2 = register 0 b1

topEntityI
  :: forall domA domB
   . ( HiddenClockResetEnable domA
     , HiddenClockResetEnable domB )
  => Signal domA (Unsigned 16)
  -> Signal domB (Unsigned 16)
  -> Signal domA (Unsigned 16)
topEntityI a b = multiClockAdd a b
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntityI #-}


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
  withSpecificClockResetEnable clkA rstA enA $ withSpecificClockResetEnable clkB rstB enB (topEntityI a) b
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal DomA Bool
testBench = done
  where
    testInputA = stimuliGenerator clkA rstA (1 :> 2 :> 3 :> 4 :> Nil)
    testInputB = stimuliGenerator clkB rstB (5 :> 6 :> 7 :> 8 :> Nil)

    expectedOutput = outputVerifier' clkA rstA (0 :> 1 :> 7 :> 9 :> 11 :> Nil)

    done = expectedOutput (topEntity clkA rstA enableGen clkB rstB enableGen testInputA testInputB)
    clkA = tbClockGen (not <$> done)
    rstA = resetGen

    clkB = tbClockGen (E.unsafeSynchronizer clkA clkB (not <$> done))
    rstB = resetGen
