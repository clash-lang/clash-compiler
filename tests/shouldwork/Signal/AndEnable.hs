module AndEnable where

import Clash.Prelude

import qualified Clash.Explicit.Prelude as E
import Clash.Explicit.Testbench

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Bool
  -> Signal System (Unsigned 8)
  -> Signal System (Unsigned 8, Unsigned 8)
topEntity clk rst gen enM =
  let f :: HiddenClockResetEnable dom
        => Signal dom Bool
        -> Signal dom (Unsigned 8)
        -> Signal dom (Unsigned 8, Unsigned 8)
      f enM0 i = bundle (register 0 i, andEnable enM0 $ register 0 i)
  in withClockResetEnable clk rst gen (f enM)

testBench
  :: Signal System Bool
testBench = done
 where
  gen  = True :> True :> False :> True :> True  :> True :> False :> True :> Nil
  enM = True :> True :> False :> True :> False :> True :> Nil
  gen0 = toEnable $ stimuliGenerator clk rst gen
  enM0 = stimuliGenerator clk rst enM
  inp = E.delay clk enableGen 0 $ inp + 1
  expectedOutput = outputVerifier clk rst
    $    (0, 0) :> (1, 1) :> (2, 2) :> (2, 2) :> (4, 4) :>  (5, 4) :> (6, 6)
      :> (6, 6) :> (8, 8) :> Nil
  done = expectedOutput $ topEntity clk rst gen0 enM0 inp
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
