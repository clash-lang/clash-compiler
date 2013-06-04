{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fifo where

import CLaSH.Prelude
import Debug.Trace

type Elm  = Unsigned 8
type Pntr n = Unsigned (n + 1)
type Elms = Vec 4 Elm

fifo :: forall n e . (SingRep n, SingI (n+1), SingI (n^2))
     => (Pntr n, Pntr n, Vec (n^2) e)
     -> (e, Bool, Bool)
     -> ((Pntr n,Pntr n,Vec (n^2) e),(Bool,Bool,e))
fifo (rpntr, wpntr, elms) (datain,wrt,rd) = ((rpntr',wpntr',elms'),(full,empty,dataout))
  where
    wpntr' | wrt       = wpntr + 1
           | otherwise = wpntr
    rpntr' | rd        = rpntr + 1
           | otherwise = rpntr

    mask  = resizeU (maxBound :: Unsigned n)
    wind  = wpntr .&. mask
    rind  = rpntr .&. mask

    elms' | wrt       = vreplace elms wind datain
          | otherwise = elms

    n = fromInteger $ fromSing (sing :: Sing n)

    empty = wpntr == rpntr
    full  = (testBit wpntr n) /= (testBit rpntr n) &&
            (wind == rind)

    dataout = vindex elms rind

fifoL :: Comp (Elm,Bool,Bool) (Bool,Bool,Elm)
fifoL = fifo ^^^ (0,0,vcopyE (sing :: Sing 4) 0)

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
