module CalculatorArrow where

import CLaSH.Prelude
import Control.Arrow

type Word = Signed 4
data OPC a = ADD | MUL | Imm a | Pop | Push

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) a b = f (g a b)

infixr 9 .:

alu ADD     = Just .: (+)
alu MUL     = Just .: (*)
alu (Imm i) = const . const (Just i)
alu _       = const . const Nothing

pu alu (op1,op2,cnt) (dmem,Pop)  = ((dmem,op1,cnt-1),(cnt,Nothing))
pu alu (op1,op2,cnt) (dmem,Push) = ((op1,op2,cnt+1) ,(cnt,Nothing))
pu alu (op1,op2,cnt) (dmem,opc)  = ((op1,op2,cnt)   ,(cnt,alu opc op1 op2))

datamem mem (addr,Nothing)  = (mem                  ,mem ! addr)
datamem mem (addr,Just val) = (vreplace mem addr val,mem ! addr)

topEntity :: Comp (OPC Word) (Maybe Word)
topEntity = proc i -> do
    rec (addr,val) <- (pu alu ^^^ (0,0,0 :: Unsigned 3)) -< (mem,i)
        mem        <- (datamem ^^^ initMem)              -< (addr,val)
    returnA -< val
  where
    initMem    = vcopy (snat :: SNat 8) 0

testInput :: [OPC Word]
testInput = [Imm 1,Push,Imm 2,Push,Pop,Pop,Pop,ADD]

expectedOutput :: [Maybe Word]
expectedOutput = [Just 1,Nothing,Just 2,Nothing,Nothing,Nothing,Nothing,Just 3]
