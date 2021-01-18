module T1632 where
import Clash.Prelude

data MCause
  = MCause (BitVector 4)
  deriving (Generic, NFDataX)
deriveAutoReg ''MCause

topEntity :: SystemClockResetEnable => Signal System MCause -> Signal System MCause
topEntity = autoReg (MCause 0)
