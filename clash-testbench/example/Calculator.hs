{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Calculator where

import Clash.Prelude hiding (Word)

type Word = Signed 4
data OPC a = ADD | MUL | Imm a | Pop | Push
  deriving (Lift, Generic, BitPack, NFDataX, Show)

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
pu _ (op1,   _, cnt) (dmem, Pop)  = ((dmem, op1, cnt - 1), (cnt, Nothing)      )
pu _ (op1, op2, cnt) (   _, Push) = ((op1, op2, cnt + 1) , (cnt, Nothing)      )
pu a (op1, op2, cnt) (   _, opc)  = ((op1, op2, cnt)     , (cnt, a opc op1 op2))

datamem :: (KnownNat n, Integral i)
        => Vec n a       -- Current state
        -> (i, Maybe a)  -- Input
        -> (Vec n a, a)  -- (New state, Output)
datamem mem (addr,Nothing)  = (mem                 ,mem !! addr)
datamem mem (addr,Just val) = (replace addr val mem,mem !! addr)

topEntity
  :: Clock  System
  -> Reset  System
  -> Enable System
  -> Signal System (OPC Word)
  -> Signal System (Maybe Word)
topEntity = exposeClockResetEnable go where
  go i = val where
    (addr,val) = (pu alu <^> (0,0,0 :: Unsigned 3)) (mem,i)
    mem        = (datamem <^> initMem) (addr,val)
    initMem    = replicate d8 0
{-# NOINLINE topEntity #-}
