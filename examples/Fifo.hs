{-# LANGUAGE ScopedTypeVariables #-}
module Fifo where

import Clash.Prelude

type Elm  = Unsigned 8
type Pntr n = Unsigned (n + 1)
type Elms = Vec 4 Elm

fifo :: forall n e . (KnownNat n, KnownNat (n+1), KnownNat (n+1+1)
                     ,KnownNat (n+1+2), KnownNat (2^n))
     => (Pntr n, Pntr n, Vec (2^n) e)
     -> (e, Bool, Bool)
     -> ((Pntr n,Pntr n,Vec (2^n) e),(Bool,Bool,e))
fifo (rpntr, wpntr, elms) (datain,wrt,rd) = ((rpntr',wpntr',elms'),(full,empty,dataout))
  where
    wpntr' | wrt       = wpntr + 1
           | otherwise = wpntr
    rpntr' | rd        = rpntr + 1
           | otherwise = rpntr

    mask  = resize (maxBound :: Unsigned n)
    wind  = wpntr .&. mask
    rind  = rpntr .&. mask

    elms' | wrt       = replace wind datain elms
          | otherwise = elms

    n = fromInteger $ snatToInteger (SNat :: SNat n)

    empty = wpntr == rpntr
    full  = (testBit wpntr n) /= (testBit rpntr n) &&
            (wind == rind)

    dataout = elms !! rind

fifoL
  :: SystemClockResetEnable
  => Signal System (Elm,Bool,Bool)
  -> Signal System (Bool,Bool,Elm)
fifoL = fifo `mealy` (0,0,replicate d4 0)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Elm,Bool,Bool)
  -> Signal System (Bool,Bool,Elm)
topEntity = exposeClockResetEnable fifoL

testdatas :: [[(Elm,Bool,Bool)]]
testdatas = [
  -- write an element, wait one cycle, write and read, wait a cycle ->
  [(1,True,False), (2,False,False), (3, True,True), (4,False,False)],
  -- fill up fifo firs then empty it again
  [(1,True,False), (2,True,False), (3,True,False), (4,True,False), (5,False,True), (6,False,True),(7,False,True),(8,False,True),(9,False,False)]
  ]

-- Expected value for different testdata inputs
express :: [[(Bool, Bool, Elm)]]
express = [
  [(False,True,0),(False,False,1),(False,False,1),(False,False,3)],
  [(False,True,0),(False,False,1),(False,False,1),(False,False,1),(True,False,1),(False,False,2),(False,False,3),(False,False,4),(False,True,1)]
  ]
