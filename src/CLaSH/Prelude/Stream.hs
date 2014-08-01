{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module CLaSH.Prelude.Stream
  ( Valid
  , Ready
  , STREAM (..)
  , fifo
  )
where

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Default
import Prelude hiding ((.),id)
import GHC.TypeLits

import CLaSH.Promoted.Nat
import CLaSH.Signal.Implicit
import CLaSH.Sized.Index
import CLaSH.Sized.Vector

type Valid = Bool
type Ready = Bool

-- | A simple streaming interface with back-pressure
--
-- A component adhering to the 'STREAM' interface has three inputs:
--
--   * A control input indicating that there is the data input contains valid data
--   * A control input indicating that the component connected to the output is
--     ready to receive new data
--   * The data input
--
-- It also has three outputs:
--
--   * A control output indicating that its output is valid
--   * A control output indicating that it is ready to receive new data
--   * A data output
--
-- Two stream interfaces are connected like so:
--
-- > <---------------------------+   +------------------------------
-- >           component1        |   |         component2
-- >      /-------------------\  |   |   /-------------------\
-- > ---->|validIn    readyOut|--+   +-->|readyIn     dataOut|----->
-- >      |                   |          |                   |
-- > ---->|dataIn     validOut|--------->|validIn    validOut|----->
-- >      |                   |          |                   |
-- >  +-->|readyIn     dataOut|--------->|dataIn     readyOut|---+
-- >  |   \-------------------/          \-------------------/   |
-- >  |                                                          |
-- >  +----------------------------<-----------------------------+
--
-- In code this is done as:
--
-- > compose (STREAM component1) (STREAM component2) = (STEAM component3)
-- >   where
-- >    component3 validIn1 readyIn2 dataIn1 = (validOut2, readyOut1, dataOut2)
-- >      where
-- >        (validOut1,readyOut1,dataOut1) = component1 validIn1 readyOut2 dataIn1
-- >        (validOut2,readyOut2,dataOut2) = component2 validOut1 readyIn2 dataOut1

newtype STREAM i o
  = STREAM
  { runFIFO :: Signal Valid
            -> Signal Ready
            -> Signal i
            -> (Signal Valid, Signal Ready, Signal o)
  }

instance Category STREAM where
  id = STREAM (\valid ready dataIn -> (valid,ready,dataIn))
  (STREAM f2) . (STREAM f1) = STREAM f3
    where
      f3 f1ValIn f2ReadyIn f1DataIn = (f2ValOut,f1ReadyOut,f2DataOut)
        where
          (f1ValOut,f1ReadyOut,f1DataOut) = f1 f1ValIn  f2ReadyOut f1DataIn
          (f2ValOut,f2ReadyOut,f2DataOut) = f2 f1ValOut f2ReadyIn  f1DataOut

instance Arrow STREAM where
  arr f = STREAM (\valid ready dataIn -> (valid,ready,f <$> dataIn))

  first (STREAM f) = STREAM f'
    where
      f' valIn readyIn dataIn = (valOut,readyOut,pack (dOut,dInR))
        where
          (dInL,dInR)            = unpack dataIn
          (valOut,readyOut,dOut) = f valIn readyIn dInL

  second (STREAM f) = STREAM f'
    where
      f' valIn readyIn dataIn = (valOut,readyOut,pack (dInL,dOut))
        where
          (dInL,dInR)            = unpack dataIn
          (valOut,readyOut,dOut) = f valIn readyIn dInR

  (STREAM f1) *** (STREAM f2) = STREAM f3
    where
      f3 valIn readyIn dataIn = ( (&&) <$> f1ValOut   <*> f2ValOut
                                , (&&) <$> f1ReadyOut <*> f2ReadyOut
                                , pack (f1DataOut,f2DataOut)
                                )
        where
          (dInL,dInR)                     = unpack dataIn
          (f1ValOut,f1ReadyOut,f1DataOut) = f1 valIn readyIn dInL
          (f2ValOut,f2ReadyOut,f2DataOut) = f2 valIn readyIn dInR

  (STREAM f1) &&& (STREAM f2) = STREAM f3
    where
      f3 valIn readyIn dataIn = ( (&&) <$> f1ValOut   <*> f2ValOut
                                , (&&) <$> f1ReadyOut <*> f2ReadyOut
                                , pack (f1DataOut,f2DataOut)
                                )
        where
          (f1ValOut,f1ReadyOut,f1DataOut) = f1 valIn readyIn dataIn
          (f2ValOut,f2ReadyOut,f2DataOut) = f2 valIn readyIn dataIn

instance ArrowLoop STREAM where
  loop (STREAM f) = STREAM g
    where
      g valIn readyIn b = (fValOut,fReadyOut,c)
        where
          (fValOut,fReadyOut,fDataOut) = f valid ready (pack (b,d))
          valid = (&&) <$> valIn   <*> fValOut
          ready = (&&) <$> readyIn <*> fReadyOut
          (c,d) = unpack fDataOut


fifoT :: (KnownNat (n + 1), KnownNat (n + 2))
      => (Vec (n + 1) a, Index (n + 2))   -- ^ (FIFO Queue, content counter)
      -> Valid                            -- ^ Input is valid
      -> Ready                            -- ^ Output is ready to receive values
      -> a                                -- ^ Data input
      -> ( (Vec (n + 1) a, Index (n + 2)) -- (FIFO Queue, content counter)
         , ( Valid                        -- Output is valid
           , Ready                        -- FIFO ready for new values
           , a                            -- Data output
           )
         )
fifoT (queue,cntr) inputValid outputReady dataIn =
    ((queue',cntr'), (queueValid, queueReady, dataOut))
  where
    -- Derived input signals
    outputNotReady = not outputReady
    inputInvalid   = not inputValid

    -- Assertions about the queue
    emptyQueue    = cntr == 0
    nonEmptyQueue = not emptyQueue
    nonFullQueue  = toInteger cntr /= vlength queue

    -- Control signal for 'cntr' (and derived 'rdpointer')
    -- * perfromEnqueue: increment number of elements in the queue when there is
    --   a valid input, the queue is not full, and the output is not ready to
    --   receive new values.
    -- * perfromShift: decrement number of elements in the queue when there is
    --   no valid input, the queue is not empty, and the output is ready to
    --   receive new values.
    perfromEnqueue = inputValid   && nonFullQueue  && outputNotReady
    perfromShift   = inputInvalid && nonEmptyQueue && outputReady

    -- Position to read: "number of values in the queue" - 1. saturated to 0.
    rdpointer = if cntr == 0 then 0 else cntr - 1

    -- Number of elements in the queue
    cntr' | perfromEnqueue = cntr + 1
          | perfromShift   = rdpointer -- See above
          | otherwise      = cntr

    -- Add valid values to the queue if it is ready for new values
    queue' | queueReady && inputValid = queue <<+ dataIn
           | otherwise                = queue

    -- Output is valid is we have elements in the queue, or the input is valid
    queueValid = nonEmptyQueue || inputValid
    -- FIFO is ready when the queue is not full, or the output is ready
    queueReady = nonFullQueue  || outputReady

    -- If we have an empty queue, the input skips the queue straight to the
    -- output
    dataOut | emptyQueue   = dataIn
            | otherwise    = queue ! rdpointer

-- | Zero-delay FIFO queue of @(n + 1)@ elements.
--
-- * Zero-delay: when the queue is empty, valid inputs are immediately routed to
--   the output, skipping the queue.
-- * Forgets new inputs when the queue is full (as opposed to forgetting the
--   oldest element)
fifo :: (KnownNat (n + 1), KnownNat (n + 2), Default a)
     => SNat (n + 1) -- ^ Number of elements in the FIFO queue
     -> STREAM a a     -- ^ A FIFO adhering to the 'STREAM' interface
fifo sz = STREAM fifo'
  where
    fifo' valIn readyIn dataIn = unpack fOut
      where
        (queue',fOut) = unpack (fifoT <$> queue <*> valIn <*> readyIn <*> dataIn)
        queue         = register (vcopy sz def,0) queue'
