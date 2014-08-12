{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Arrows       #-}

module CLaSH.Prelude.Stream
  ( STREAM (..)
  , Valid
  , Ready
  , fifo
  , fifoIC
  , fifoZero
  , fifoZeroIC
  ,runTest
  )
where

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Default
import Prelude hiding ((.),id)
import GHC.TypeLits

import CLaSH.Promoted.Nat
import CLaSH.Signal
import CLaSH.Sized.Index
import CLaSH.Sized.Vector

import CLaSH.Promoted.Nat.Literals
import Debug.Trace

type Valid = Bool
type Ready = Bool

-- | A simple streaming interface with back-pressure
--
-- A component adhering to the 'STREAM' interface has three inputs:
--
--   * A control input indicating that there the data input contains valid data
--   * A control input indicating that the component connected to the output is
--     ready to receive new data
--   * The data input
--
-- It also has three outputs:
--
--   * A control output indicating that its output is valid
--   * A control output indicating that it is ready to receive new data
--   * The data output
--
-- When we compose two components adhering to the 'STREAM' interface, the
-- @valid@ and @data@ signals flow from left to right, while the @ready@
-- signals flow from right to left:
--
-- @
-- (STREAM component1) >>> (STREAM component2) = (STEAM component3)
--   where
--    component3 validIn1 readyIn2 dataIn1 = (validOut2, readyOut1, dataOut2)
--      where
--        (validOut1,readyOut1,dataOut1) = component1 validIn1 readyOut2 dataIn1
--        (validOut2,readyOut2,dataOut2) = component2 validOut1 readyIn2 dataOut1
-- @
--
-- When viewed as a circuit diagram, @>>>@ looks like:
--
-- <<doc/streamcompose.svg>>
--
-- 'STREAM' is an instance of 'Category', 'Arrow', and 'ArrowLoop', meaning
-- you can define compositions of components adhering to the 'STREAM' inferface
-- using the arrow syntax:
--
-- @
-- sequenceAndLoop :: STREAM Int Int
-- sequenceAndLoop = proc a -> do
--   rec b     <- component1           -< (a,d)
--       c     <- component2           -< b
--       (d,e) <- component3           -< c
--   returnA -< e
-- @
--
-- Which gives rise to the following circuit:
--
-- <<doc/streamarrowcompose.svg>>

newtype STREAM i o
  = STREAM
  { runSTREAM :: Signal Valid
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


fifoT :: (KnownNat n, KnownNat (n + 1), Show a)
      => (Vec n a, Index (n + 1))
      -> Valid
      -> Ready
      -> a
      -> ( (Vec n a, Index (n + 1))
         , ( Valid
           , Ready
           , a
           )
         )
fifoT (queue,cntr) inputValid outputReady dataIn = trace (show (queue,cntr))
    ((queue',cntr'), (queueValid, queueReady, dataOut))
  where
    -- Derived input signals
    outputNotReady = not outputReady
    inputInvalid   = not inputValid

    -- Assertions about the queue
    emptyQueue    = cntr == 0
    nonEmptyQueue = not emptyQueue
    fullQueue     = toInteger cntr == vlength queue
    nonFullQueue  = toInteger cntr /= vlength queue

    -- Control signals for 'cntr' (and derived 'rdpointer'):
    --
    -- perfromEnqueue: increment number of elements in the queue when:
    --   * The queue is not full, and,
    --   * There is a valid input, and
    --   * Either of the following holds:
    --       - The queue is empty
    --       - The output is not ready
    --
    -- perfromShift: decrement number of elements in the queue when:
    --   * The output is ready for new values, and,
    --   * Either of the following holds:
    --       - The queue is full
    --       - The input is invalid
    perfromEnqueue = nonFullQueue && inputValid  && (emptyQueue || outputNotReady)
    perfromShift   = outputReady  && (fullQueue || inputInvalid)

    -- Position to read: "number of values in the queue" - 1. saturated to 0.
    rdpointer = if cntr == 0 then 0 else cntr - 1

    -- Number of elements in the queue on the next cycle
    cntr' | perfromEnqueue = cntr + 1
          | perfromShift   = rdpointer -- See above
          | otherwise      = cntr

    -- Add valid values to the queue if it is ready for new values
    queue' | nonFullQueue && inputValid = queue <<+ dataIn
           | otherwise                  = queue

    -- Output is valid is we have elements in the queue
    queueValid = nonEmptyQueue
    -- FIFO is ready when the queue is not full
    queueReady = nonFullQueue

    -- Input is always delayed by one cycle.
    dataOut = queue ! rdpointer

-- | FIFO queue of @n@ elements.
--
-- __NB__: This FIFO has registered (buffered) outputs and is hence safe to use
-- in feedback loops.
--
-- * Forgets new inputs when the queue is full (as opposed to forgetting the
--   oldest element)
-- * The minimum delay for values wanting to pass through the FIFO is:
--   "number of values in the queue" + 1
fifo :: (KnownNat n, KnownNat (n + 1), Default a, Show a)
     => SNat n     -- ^ Number of elements in the FIFO queue
     -> STREAM a a -- ^ A FIFO adhering to the 'STREAM' interface
fifo sz = fifoIC sz Nil

-- | FIFO queue of @(m + n)@ elements, with @m@ initial elements.
--
-- __NB__: This FIFO has registered (buffered) outputs and is hence safe to use
-- in feedback loops.
--
-- * Forgets new inputs when the queue is full (as opposed to forgetting the
--   oldest element)
-- * The minimum delay for values wanting to pass through the FIFO is:
--   "number of values in the queue" + 1
fifoIC :: forall m n a . ( KnownNat m, KnownNat n, KnownNat (m + n)
                         , KnownNat (m + n + 1), Default a, Show a)
       => SNat (m + n) -- ^ Number of elements in the FIFO queue
       -> Vec  n a     -- ^ Initial elements
       -> STREAM a a   -- ^ A FIFO adhering to the 'STREAM' interface
fifoIC _ ivals = STREAM fifo'
  where
    fifo' valIn readyIn dataIn = unpack fOut
      where
        (queue',fOut) = unpack (fifoT <$> queue <*> valIn <*> readyIn <*> dataIn)
        queue         = register ((vcopyI def :: Vec m a) <++> ivals,fromInteger (vlength ivals)) queue'


fifoZeroT :: (KnownNat n, KnownNat (n + 1), Show a)
          => (Vec n a, Index (n + 1))   -- ^ (FIFO Queue, content counter)
          -> Valid                      -- ^ Input is valid
          -> Ready                      -- ^ Output is ready to receive values
          -> a                          -- ^ Data input
          -> ( (Vec n a, Index (n + 1)) -- (FIFO Queue, content counter)
             , ( Valid                  -- Output is valid
               , Ready                  -- FIFO ready for new values
               , a                      -- Data output
               )
             )
fifoZeroT (queue,cntr) inputValid outputReady dataIn = -- trace (show (queue,cntr))
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
    queueReady = nonFullQueue || outputReady

    -- If we have an empty queue, the input skips the queue straight to the
    -- output
    dataOut | emptyQueue   = dataIn
            | otherwise    = queue ! rdpointer

-- | Zero-delay FIFO queue of @n@ elements.
--
-- __WARNING__: This FIFO has circuit paths that are completely combinational.
-- When used in a feedback loop, ensure that at least one of the other
-- components in the loop has registered (buffered), @dataOut@, @validOut@, and
-- @readyOut@ lines.
--
-- * Zero-delay: when the queue is empty, valid inputs are immediately routed to
--   the output, skipping the queue. That is, the minimum delay for values
--   wanting to pass through the FIFO is:  "number of values in the queue".
-- * Forgets new inputs when the queue is full (as opposed to forgetting the
--   oldest element)
fifoZero :: (KnownNat n, KnownNat (n + 1), Default a, Show a)
     => SNat n     -- ^ Number of elements in the FIFO queue
     -> STREAM a a -- ^ A FIFO adhering to the 'STREAM' interface
fifoZero sz = fifoZeroIC sz Nil

-- | Zero-delay FIFO queue of @(m + n)@ elements, with @m@ initial elements.
--
-- __WARNING__: This FIFO has circuit paths that are completely combinational.
-- When used in a feedback loop, ensure that at least one of the other
-- components in the loop has registered (buffered), @dataOut@, @validOut@, and
-- @readyOut@ lines.
--
-- * Zero-delay: when the queue is empty, valid inputs are immediately routed to
--   the output, skipping the queue. That is, the minimum delay for values
--   wanting to pass through the FIFO is:  "number of values in the queue".
-- * Forgets new inputs when the queue is full (as opposed to forgetting the
--   oldest element)
fifoZeroIC :: forall m n a . ( KnownNat m, KnownNat n, KnownNat (m + n)
                         , KnownNat (m + n + 1), Default a, Show a)
       => SNat (m + n) -- ^ Number of elements in the FIFO queue
       -> Vec  n a     -- ^ Initial elements
       -> STREAM a a   -- ^ A FIFO adhering to the 'STREAM' interface
fifoZeroIC _ ivals = STREAM fifo'
  where
    fifo' valIn readyIn dataIn = unpack fOut
      where
        (queue',fOut) = unpack (fifoZeroT <$> queue <*> valIn <*> readyIn <*> dataIn)
        queue         = register ((vcopyI def :: Vec m a) <++> ivals,fromInteger (vlength ivals)) queue'


test :: STREAM Int Int
test = proc a -> do
    rec b <- fifo d3 -< (a,d')
        c <- (arr g) -< b
        d' <- fifoIC d3 (0 :> Nil) -< d
        (d,e) <- fifo d3 -< c
    returnA -< e
  where
    g (p,q) = let z = p + q in (z,z)

sometimesFalseReady :: Signal Ready
-- sometimesFalseReady = fromList ([False,False] ++ repeat True)
sometimesFalseReady = fromList ([True,False,False,False] ++ repeat True)

sometimesFalseValid :: Signal Ready
sometimesFalseValid = fromList (repeat True)

runTest :: [(Int,Maybe Int)]
runTest = sampleN 50 $ pack (samples, filterValid ((&&) <$> val <*> sometimesFalseReady) dout)
  where
    (val,red,dout) = runSTREAM (fifo d3) sometimesFalseValid sometimesFalseReady samples
    samples        = extendValid ((&&) <$> sometimesFalseValid <*> red) (fromList $ cycle [1..10])

extendValid a b = fromList (f (sample a) (sample b))
  where
    f (p:ps) (q:qs) = q : if p then f ps qs else f ps (q:qs)

filterValid a b = f <$> a <*> b
  where
    f p q = if p then Just q else Nothing
