module MergeEnable where

import Clash.Prelude

import qualified Clash.Explicit.Prelude as CEP
import Clash.Explicit.Testbench

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Bool
  -> Signal System (Unsigned 8)
  -> Signal System (Unsigned 8)
topEntity clk rst en enM =
  let f :: HiddenClockResetEnable dom
        => Signal dom Bool
        -> Signal dom (Unsigned 8)
        -> Signal dom (Unsigned 8)
      f enM0 = mergeEnable enM0 $ register 0
  in withClockResetEnable clk rst en (f enM)

testBench
  :: Signal System Bool
testBench = done
 where
  enM = True :> True :> False :> True :> Nil
  en  = True :> True :> True  :> True :> False :> True :> Nil
  enM0 = stimuliGenerator clk rst enM
  en0 = toEnable $ stimuliGenerator clk rst en
  inp = CEP.delay clk enableGen 0 $ inp + 1
  expectedOutput =
    outputVerifier clk rst $ 0 :> 1 :> 2 :> 2 :> 4 :> 4 :> 6 :> Nil
  done = expectedOutput $ topEntity clk rst en0 enM0 inp
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
