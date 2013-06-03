{-# LANGUAGE DataKinds #-}
module Calculator where

import CLaSH.Prelude

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

datamem mem (addr,Nothing)  = (mem                  ,vindex mem addr)
datamem mem (addr,Just val) = (vreplace mem addr val,vindex mem addr)

topEntity :: Sync (OPC Word) -> Sync (Maybe Word)
topEntity i = val
  where
    (addr,val) = (pu alu <^> (0,0,0 :: Unsigned 3)) (mem,i)
    mem        = (datamem <^> initMem) (addr,val)
    initMem    = vcopyE (sing :: Sing 8) 0
