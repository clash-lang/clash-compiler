module T2593 where

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes

topEntity ::
  Signal System Bit ->
  Signal System Bit `Annotate` 'StringAttr "breaka" "me"
topEntity dIn =
  exposeClockResetEnable (register 0) clk rst en dIn
 where
   clk = clockGen
   rst = resetGen
   en = enableGen
