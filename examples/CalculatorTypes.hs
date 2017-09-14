module CalculatorTypes where

import Clash.Prelude

type Word = Signed 4
data OPC a = ADD | MUL | Imm a | Pop | Push
  deriving Lift
