{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Arrows #-}

module CLaSH.Prelude.Stream
  ( Stream (..)
  , sregister
  , fifo
  , fifoIC
  , fifoZero
  , fifoZeroIC
  )
where

import Control.Applicative   (Applicative (..), (<$>))
import Control.Arrow         (Arrow (..), ArrowLoop (..), ArrowChoice (..))
import Control.Category      (Category (..))
import Data.Default          (Default (..))
import Prelude               hiding ((.), (++), (!!), id, length)
import GHC.TypeLits          (KnownNat, type (+))

import CLaSH.Promoted.Nat    (SNat)
import CLaSH.Signal.Enabled  (Enabled, enabled, mapEnabled, regEn, zipWithEnabled)
import CLaSH.Signal.Explicit (CSignal, Clock (..), SClock, cregister, wrap)
import CLaSH.Signal.Internal ((&&$))
import CLaSH.Sized.Index     (Index)
import CLaSH.Sized.Vector    (Vec (..), (++), (!!), (<<+), length)

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

newtype Stream (clk :: Clock) i o
  = Stream
  { runStream :: SClock clk
              -> CSignal clk Bool
              -> CSignal clk (Enabled i)
              -> (CSignal clk Bool, CSignal clk (Enabled o))
  }

instance Category (Stream clk) where
  id = Stream (\_ r d -> (r,d))
  (Stream f) . (Stream g) = Stream h
    where
      h clk rIn dIn = (gRout,fDout)
        where
          (gRout,gDout) = g clk fRout dIn
          (fRout,fDout) = f clk rIn gDout

instance Arrow (Stream clk) where
  arr f = Stream (\_ r d -> (r,fmap (second f) d))
  first (Stream f) = Stream g
    where
      g clk rIn dIn = (rIn &&$ rOut,zipWithEnabled clk (,) l' r)
        where
          l         = mapEnabled fst dIn
          r         = mapEnabled snd dIn
          (rOut,l') = f clk rIn l

  second (Stream f) = Stream g
    where
      g clk rIn dIn = (rIn &&$ rOut,zipWithEnabled clk (,) l r')
        where
          l         = mapEnabled fst dIn
          r         = mapEnabled snd dIn
          (rOut,r') = f clk rIn r


  (Stream f) *** (Stream g) = Stream h
    where
      h clk rIn dIn = (fRout &&$ gRout, zipWithEnabled clk (,) l' r')
        where
          l = mapEnabled fst dIn
          r = mapEnabled snd dIn
          (fRout,l') = f clk rIn l
          (gRout,r') = g clk rIn r

  (Stream f) &&& (Stream g) = Stream h
    where
      h clk rIn dIn = (fRout &&$ gRout, zipWithEnabled clk (,) fDout gDout)
        where
          (fRout,fDout) = f clk rIn dIn
          (gRout,gDout) = g clk rIn dIn

instance ArrowLoop (Stream clk) where
  loop (Stream f) = Stream g
    where
      g clk rIn b = (rOut,c)
        where
          (rOut,dOut) = f clk ready (zipWithEnabled clk (,) b d)
          c           = mapEnabled fst dOut
          d           = mapEnabled snd dOut
          ready       = rIn &&$ rOut

instance ArrowChoice (Stream clk) where
  left (Stream f) = Stream g
    where
      g clk rIn dIn = (rOut,dOut)
        where
          (rOut,dOut,fRin,fDin) = wrap clk (route <$> rIn <*> dIn <*> fRout
                                                  <*> fDout)

          (fRout,fDout)         = f clk fRin fDin

      route rIn (vIn,Left a) fRout fDout =
        (fRout,second Left fDout,rIn,(vIn,a))
      route rIn (vIn,Right b) _ _ =
        (rIn,(vIn,Right b),False,(False,undefined))

  right (Stream f) = Stream g
    where
      g clk rIn dIn = (rOut,dOut)
        where
          (rOut,dOut,fRin,fDin) = wrap clk (route <$> rIn <*> dIn <*> fRout
                                                  <*> fDout)

          (fRout,fDout)         = f clk fRin fDin

      route rIn (vIn,Left a) _ _ =
        (rIn,(vIn,Left a),False,(False,undefined))
      route rIn (vIn,Right b) fRout fDout =
        (fRout,second Right fDout,rIn,(vIn,b))

  (Stream f) +++ (Stream g) = Stream h
    where
      h clk rIn dIn = (rOut,dOut)
        where
          (rOut,dOut,fRin,fDin,gRin,gDin) =
            wrap clk (route <$> rIn <*> dIn <*> fRout <*> fDout <*> gRout <*> gDout)

          (fRout,fDout) = f clk fRin fDin
          (gRout,gDout) = g clk gRin gDin

      route rIn (vIn,Left a) fRout fDout _ _ =
        (fRout,second Left fDout,rIn,(vIn,a),False,(False,undefined))

      route rIn (vIn,Right b) _ _ gRout gDout =
        (gRout,second Right gDout,False,(False,undefined),rIn,(vIn,b))

  (Stream f) ||| (Stream g) = Stream h
    where
      h clk rIn dIn = (rOut,dOut)
        where
          (rOut,dOut,fRin,fDin,gRin,gDin) =
            wrap clk (route <$> rIn <*> dIn <*> fRout <*> fDout <*> gRout <*> gDout)

          (fRout,fDout) = f clk fRin fDin
          (gRout,gDout) = g clk gRin gDin

      route rIn (vIn,Left a) fRout fDout _ _ =
        (fRout,fDout,rIn,(vIn,a),False,(False,undefined))

      route rIn (vIn,Right b) _ _ gRout gDout =
        (gRout,gDout,False,(False,undefined),rIn,(vIn,b))


sregister :: a -> Stream clk a a
sregister is = Stream sregister'
  where
    sregister' clk rIn inp = (rIn,enabled (regEn clk is inp))

fifoT :: (KnownNat n, KnownNat (n + 1))
      => (Vec n a, Index (n + 1))
      -> Bool
      -> Enabled a
      -> ( (Vec n a, Index (n + 1))
         , ( Bool
           , Enabled a
           )
         )
fifoT (queue,cntr) outputReady (inputValid,dataIn) =
    ((queue',cntr'), (queueReady, (queueValid, dataOut)))
  where
    -- Derived input signals
    outputNotReady = not outputReady
    inputInvalid   = not inputValid

    -- Assertions about the queue
    emptyQueue    = cntr == 0
    nonEmptyQueue = not emptyQueue
    fullQueue     = toInteger cntr == length queue
    nonFullQueue  = toInteger cntr /= length queue

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
    dataOut = queue !! rdpointer

-- | FIFO queue of @n@ elements.
--
-- __NB__: This FIFO has registered (buffered) outputs and is hence safe to use
-- in feedback loops.
--
-- * Forgets new inputs when the queue is full (as opposed to forgetting the
--   oldest element)
-- * The minimum delay for values wanting to pass through the FIFO is:
--   "number of values in the queue" + 1
fifo :: (KnownNat n, KnownNat (n + 1), Default a)
     => SNat n         -- ^ Number of elements in the FIFO queue
     -> Stream clk a a -- ^ A FIFO adhering to the 'STREAM' interface
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
fifoIC :: forall m n a clk . (KnownNat m, KnownNat n, KnownNat (m + n),
                              KnownNat (m + n + 1), Default a)
       => SNat (m + n)    -- ^ Number of elements in the FIFO queue
       -> Vec  n a        -- ^ Initial elements
       -> Stream clk a a  -- ^ A FIFO adhering to the 'STREAM' interface
fifoIC _ ivals = Stream fifo'
  where
    fifo' clk readyIn dataIn = wrap clk fOut
      where
        (queue',fOut) = wrap clk
                          (fifoT <$> queue <*> readyIn <*> dataIn)
        queue         = cregister clk
                          ( (def :: Vec m a) ++ ivals
                          , fromInteger (length ivals))
                          queue'


fifoZeroT :: (KnownNat n, KnownNat (n + 1))
          => (Vec n a, Index (n + 1))   -- ^ (FIFO Queue, content counter)
          -> Bool                       -- ^ Output is ready to receive values
          -> Enabled a                  -- ^ Data input
          -> ( (Vec n a, Index (n + 1)) -- (FIFO Queue, content counter)
             , ( Bool                   -- FIFO ready for new values
               , Enabled a              -- Data output
               )
             )
fifoZeroT (queue,cntr) outputReady (inputValid,dataIn) =
    ((queue',cntr'), (queueReady, (queueValid, dataOut)))
  where
    -- Derived input signals
    outputNotReady = not outputReady
    inputInvalid   = not inputValid

    -- Assertions about the queue
    emptyQueue    = cntr == 0
    nonEmptyQueue = not emptyQueue
    nonFullQueue  = toInteger cntr /= length queue

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
            | otherwise    = queue !! rdpointer

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
     => SNat n         -- ^ Number of elements in the FIFO queue
     -> Stream clk a a -- ^ A FIFO adhering to the 'STREAM' interface
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
fifoZeroIC :: forall m n a clk . ( KnownNat m, KnownNat n, KnownNat (m + n),
                                   KnownNat (m + n + 1), Default a)
       => SNat (m + n) -- ^ Number of elements in the FIFO queue
       -> Vec  n a     -- ^ Initial elements
       -> Stream clk a a   -- ^ A FIFO adhering to the 'STREAM' interface
fifoZeroIC _ ivals = Stream fifo'
  where
    fifo' clk readyIn dataIn = wrap clk fOut
      where
        (queue',fOut) = wrap clk
                          (fifoZeroT <$> queue <*> readyIn <*> dataIn)
        queue         = cregister clk
                          ( (def :: Vec m a) ++ ivals
                            ,fromInteger (length ivals))
                          queue'

-- -- test :: STREAM Int Int
-- -- test = proc a -> do
-- --     rec b <- fifo d3 -< (a,d')
-- --         c <- (arr g) -< b
-- --         d' <- fifoIC d3 (0 :> Nil) -< d
-- --         (d,e) <- fifo d3 -< c
-- --     returnA -< e
-- --   where
-- --     g (p,q) = let z = p + q in (z,z)

-- -- sometimesFalseReady :: Signal Ready
-- -- -- sometimesFalseReady = fromList ([False,False] ++ repeat True)
-- -- sometimesFalseReady = fromList ([True,False,False,False] ++ repeat True)

-- -- sometimesFalseValid :: Signal Ready
-- -- sometimesFalseValid = fromList (repeat True)

-- -- runTest :: [(Int,Maybe Int)]
-- -- runTest = sampleN 50 $ pack (samples, filterValid ((&&) <$> val <*> sometimesFalseReady) dout)
-- --   where
-- --     (val,red,dout) = runSTREAM (fifo d3) sometimesFalseValid sometimesFalseReady samples
-- --     samples        = extendValid ((&&) <$> sometimesFalseValid <*> red) (fromList $ cycle [1..10])

-- -- extendValid a b = fromList (f (sample a) (sample b))
-- --   where
-- --     f (p:ps) (q:qs) = q : if p then f ps qs else f ps (q:qs)

-- -- filterValid a b = f <$> a <*> b
-- --   where
-- --     f p q = if p then Just q else Nothing
