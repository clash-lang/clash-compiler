module T2593 where

import Clash.Annotations.SynthesisAttributes
import Clash.Prelude

topEntity ::
  Signal System Bit ->
  Signal System Bit `Annotate` 'StringAttr "break" "me"
topEntity dIn =
  exposeClockResetEnable (register 0) clk rst en dIn
 where
   clk = clockGen
   rst = resetGen
   en = enableGen
