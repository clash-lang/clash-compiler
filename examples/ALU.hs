module ALU where

import Clash.Prelude

data OPC = ADD | MUL | SUB

topEntity :: OPC -> Integer -> Integer -> Integer
topEntity SUB = (-)
topEntity ADD = (+)
topEntity MUL = (*)
