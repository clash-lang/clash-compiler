module Calculator where

import Clash.Prelude hiding (Word)
import CalculatorTypes

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) a b = f (g a b)

infixr 9 .:

alu :: Num a => OPC a -> a -> a -> Maybe a
alu ADD     = Just .: (+)
alu MUL     = Just .: (*)
alu (Imm i) = const . const (Just i)
alu _       = const . const Nothing

pu :: (Num a, Num b)
   => (OPC a -> a -> a -> Maybe a)
   -> (a, a, b)       -- Current state
   -> (a, OPC a)      -- Input
   -> ( (a, a, b)     -- New state
      , (b, Maybe a)  -- Output
      )
pu alu (op1,op2,cnt) (dmem,Pop)  = ((dmem,op1,cnt-1),(cnt,Nothing))
pu alu (op1,op2,cnt) (dmem,Push) = ((op1,op2,cnt+1) ,(cnt,Nothing))
pu alu (op1,op2,cnt) (dmem,opc)  = ((op1,op2,cnt)   ,(cnt,alu opc op1 op2))

datamem :: (KnownNat n, Integral i)
        => Vec n a       -- Current state
        -> (i, Maybe a)  -- Input
        -> (Vec n a, a)  -- (New state, Output)
datamem mem (addr,Nothing)  = (mem                 ,mem !! addr)
datamem mem (addr,Just val) = (replace addr val mem,mem !! addr)

topEntity
  :: SystemClockReset
  => Signal System (OPC Word)
  -> Signal System (Maybe Word)
topEntity i = val
  where
    (addr,val) = (pu alu <^> (0,0,0 :: Unsigned 3)) (mem,i)
    mem        = (datamem <^> initMem) (addr,val)
    initMem    = replicate (SNat :: SNat 8) 0
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = stimuliGenerator $(listToVecTH [Imm 1::OPC Word,Push,Imm 2,Push,Pop,Pop,Pop,ADD])
    expectedOutput = outputVerifier   $(listToVecTH [Just 1 :: Maybe Word,Nothing,Just 2,Nothing,Nothing,Nothing,Nothing,Just 3])
    done           = expectedOutput (topEntity testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
