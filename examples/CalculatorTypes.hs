module CalculatorTypes where

import CLaSH.Prelude

type Word = Signed 4
data OPC a = ADD | MUL | Imm a | Pop | Push

deriveLift ''OPC
