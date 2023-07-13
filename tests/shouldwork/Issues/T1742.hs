module T1742 where

import           Clash.Prelude
import           Clash.Signal

import           Clash.Annotations.SynthesisAttributes
import           Clash.Annotations.TopEntity
import           Clash.Annotations.TH

createDomain vSystem{vName="Bank2C", vPeriod=20000, vResetKind=Asynchronous, vResetPolarity=ActiveLow}

type Pin t x y = t `Annotate` 'StringAttr "chip_pin" x
                   `Annotate` 'StringAttr "altera_attribute" ("-name IO_STANDARD \"" `AppendSymbol` y `AppendSymbol` "\"")

top :: HiddenClockResetEnable dom => Signal dom (BitVector 4)
top = s where s = register 0 (s + 1)

topEntity
  :: "CLK_50_B2C"  ::: Pin (Clock Bank2C) "PIN_AW38" "1.2 V"
  -> "CPU_RESET_n" ::: Signal Bank2C Bool
  -> "LED"         ::: Signal Bank2C (BitVector 4)
topEntity clk rstnButton = exposeClockResetEnable top clk rst en where
  en = enableGen
  rst = unsafeFromActiveLow rstnButton
  rstSync = resetSynchronizer clk rst

makeTopEntityWithName 'topEntity "shell"
