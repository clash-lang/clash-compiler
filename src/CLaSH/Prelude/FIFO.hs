{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module CLaSH.Prelude.FIFO where

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Default
import Prelude hiding ((.),id)
import GHC.TypeLits

import CLaSH.Promoted.Nat
import CLaSH.Promoted.Nat.Literals
import CLaSH.Signal.Implicit
import CLaSH.Sized.Index
import CLaSH.Sized.Vector

import Debug.Trace

newtype FIFO i o = FIFO { runFIFO :: Signal Bool -> Signal Bool -> Signal i -> (Signal Bool, Signal Bool, Signal o) }

instance Category FIFO where
  id = FIFO (\valid ready dataIn -> (valid,ready,dataIn))
  (FIFO f2) . (FIFO f1) = FIFO f3
    where
      f3 f1ValIn f2ReadyIn f1DataIn = (f2ValOut,f1ReadyOut,f2DataOut)
        where
          (f1ValOut,f1ReadyOut,f1DataOut) = f1 f1ValIn  f2ReadyOut f1DataIn
          (f2ValOut,f2ReadyOut,f2DataOut) = f2 f1ValOut f2ReadyIn  f1DataOut

instance Arrow FIFO where
  arr f = FIFO (\valid ready dataIn -> (valid,ready,f <$> dataIn))

  first (FIFO f) = FIFO f'
    where
      f' valIn readyIn dataIn = (valOut,readyOut,pack (dOut,dInR))
        where
          (dInL,dInR)            = unpack dataIn
          (valOut,readyOut,dOut) = f valIn readyIn dInL

  second (FIFO f) = FIFO f'
    where
      f' valIn readyIn dataIn = (valOut,readyOut,pack (dInL,dOut))
        where
          (dInL,dInR)            = unpack dataIn
          (valOut,readyOut,dOut) = f valIn readyIn dInR

  (FIFO f1) *** (FIFO f2) = FIFO f3
    where
      f3 valIn readyIn dataIn = ( (&&) <$> f1ValOut <*> f2ValOut
                                , (&&) <$> f1ReadyOut <*> f2ReadyOut
                                , pack (f1DataOut,f2DataOut)
                                )
        where
          (dInL,dInR)                     = unpack dataIn
          (f1ValOut,f1ReadyOut,f1DataOut) = f1 valIn readyIn dInL
          (f2ValOut,f2ReadyOut,f2DataOut) = f2 valIn readyIn dInR

  (FIFO f1) &&& (FIFO f2) = FIFO f3
    where
      f3 valIn readyIn dataIn = ( (&&) <$> f1ValOut <*> f2ValOut
                                , (&&) <$> f1ReadyOut <*> f2ReadyOut
                                , pack (f1DataOut,f2DataOut)
                                )
        where
          (f1ValOut,f1ReadyOut,f1DataOut) = f1 valIn readyIn dataIn
          (f2ValOut,f2ReadyOut,f2DataOut) = f2 valIn readyIn dataIn

fifoT :: KnownNat (n + 1) => Default a => Show a
      => (Vec (n + 1) a, Index (n + 1), Bool, Bool) -- ^ FIFO Queue
      -> Bool                           -- ^ Input is valid
      -> Bool                           -- ^ Output is ready to receive values
      -> a                              -- ^ Data input
      -> ( (Vec (n + 1) a, Index (n + 1), Bool, Bool) -- New FIFO Queue
         , ( Bool                         -- Output is valid
           , Bool                         -- FIFO can receive new values
           , a                            -- Data output
           )
         )
fifoT (queue,cntr,queueReady,emptyQueue) inputValid outputReady dataIn =
      ((queue'',cntr',queueReady',emptyQueue'),(queueValid,queueReady,dataOut))
  where
    -- Derived input status signals
    outputNotReady = not outputReady

    -- Queue assertions
    emptyQueue'    = cntr == 0
    nonEmptyQueue  = not emptyQueue
    shifting       = nonEmptyQueue && outputReady

    -- Queue status signals
    queueValid     = nonEmptyQueue || inputValid
    queueReady'    = toInteger cntr /= maxIndex queue

    -- Counter logic:
    -- * Queue ready for new data, but the output is not: cntr' = cntr + 1
    -- * There is data in the queue, and the output is ready: cntr' = cntr - 1
    -- * Either, the queue is full and the output is not ready, or, the queue
    --   is empty and the output is ready: cntr' = cntr
    cntr' | queueReady' && outputNotReady = cntr + 1
          | shifting                      = cntr - 1
          | otherwise                     = cntr

    -- Queue logic:
    -- * When there is valid input, write it at the current write pointer
    --   position.
    -- * If the output is ready for data, and there is data in the queue, shift
    --   out the earliest received value
    queue'  | inputValid && queueReady     = vreplace queue cntr dataIn
            | otherwise                    = queue
    queue'' | shifting                     = def +>> queue'
            | otherwise                    = queue'

    dataOut | emptyQueue = dataIn
            | otherwise  = vlast queue


fifo :: KnownNat (n + 1) => Default a => Show a
     => SNat (n + 1)
     -> FIFO a a
fifo sz = FIFO fifo'
  where
    fifo' valIn readyIn dataIn = unpack fOut
      where
        (queue',fOut) = unpack (fifoT <$> queue <*> valIn <*> readyIn <*> dataIn)
        queue         = register (vcopy sz def,0,True,True) queue'


alwaysFalse :: Signal Bool
alwaysFalse = pure False

alwaysTrue :: Signal Bool
alwaysTrue  = pure True

sometimesFalse :: Signal Bool
sometimesFalse = fromList (cycle [False,True,True])

halfTrue :: Signal Bool
halfTrue = fromList (cycle [True,True,False,False])

counter :: Signal Int
counter = fromList [1,2,99,99,3,4,99,99,5,6,99,99,7,8,99,99,9,10]

filterReady r d = filterReadyT <$> r <*> d
  where
    filterReadyT True a = Just a
    filterReadyT _    _ = Nothing

test :: [Maybe Int]
test = sampleN 10 nice
  where
    (v,r,dout) = runFIFO (fifo d2) halfTrue sometimesFalse counter
    nice       = filterReady ((&&) <$> sometimesFalse <*> v) dout
