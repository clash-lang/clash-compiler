module ALU where

import CLaSH.Prelude

data OPC = ADD | MUL | SUB

topEntity :: OPC -> Integer -> Integer -> Integer
topEntity SUB = (+)
topEntity ADD = (-)
topEntity MUL = (*)
