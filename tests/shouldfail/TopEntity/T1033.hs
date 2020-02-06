module T1033 where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClock, hasClock)
import qualified Prelude as P

{-# ANN topEntity
  (Synthesize
    { t_name   = "top"
    , t_inputs = [ PortName "wrong"]
    , t_output = PortName "theOutput"
    }
  )#-}
topEntity
  :: (Clock System, Reset System, Enable System)
  -> Signal System Int
  -> Signal System Int
topEntity (clk, rst, en) i =
  register clk rst en 0 i

