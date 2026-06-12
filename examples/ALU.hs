module ALU where

import Clash.Prelude

data OPC = ADD | MUL | SUB

topEntity :: OPC -> Int -> Int -> Int
topEntity SUB = (-)
topEntity ADD = (+)
topEntity MUL = (*)
