module SigmaDeltaADC where

import Clash.Prelude hiding (filter, truncate)
import Clash.Sized.Internal.BitVector

-- | Remove the LSBs from the accumulator output when the accumulator is ready
-- This is used for both the accumulator and the filter
truncateAccum ::
  forall adcWdith accumExtra.
  (KnownNat adcWdith, KnownNat accumExtra) =>
  -- | The previous accum
  BitVector adcWdith ->
  -- | parts of the tuple
  --
  -- 1. Accumulator output
  -- 2. Boolean for when the accumulator is ready
  (BitVector (adcWdith + accumExtra), Bool) ->
  -- | parts of tuple
  --
  -- 1. updated accum
  -- 2. output accum
  (BitVector adcWdith, BitVector adcWdith)
truncateAccum accum (sigma, rollover)
  | rollover = (fst (split sigma), accum)
  | otherwise = (accum, accum)


accumulator ::
  forall adcWidth accumExtra inputWidth dom.
  ( HiddenClockResetEnable dom,
    KnownNat accumExtra,
    KnownNat adcWidth,
    KnownNat inputWidth,
    inputWidth <= (adcWidth + accumExtra)
  ) =>
  -- | The accumulator width
  SNat (accumExtra + adcWidth) ->
  -- | The amount of ones from the input
  Signal dom (BitVector inputWidth) ->
  -- | Parts of the Tuple
  --
  -- 1. The accumulator output
  -- 2. Accumulator ready going to the filter
  ( Signal dom (BitVector adcWidth),
    Signal dom Bool
  )
accumulator _ numOnes = (accum, rollover)
  where
    accum = mealyB truncateAccum 0 (sigma, rollover)
    (sigma, rollover) =
        mealyB accumulatorCounter (accumInit , False, 0, 0) numOnes
    accumInit = 0 :: BitVector (accumExtra + adcWidth + 1)

-- | The counter for sigma, which counts counts the number of ones from
-- the input and resets when count is equal to zero
accumulatorCounter ::
  forall sigmaWidth inputWidth.
  (KnownNat sigmaWidth, KnownNat inputWidth, inputWidth <= sigmaWidth) =>
  -- | Parts of the Tuple
  --
  -- 1. The previous sigma
  -- 2. bool for when sigma is ready
  -- 3. previous number of ones from the input
  -- 4. previous value of the main counter-> BitVector n1
  ( BitVector (sigmaWidth + 1),
    Bool,
    BitVector inputWidth,
    Unsigned (sigmaWidth - inputWidth + 1)
  ) ->
  -- | number of ones from the input
  BitVector inputWidth ->
  -- | Parts of the Tuple
  --
  -- 1. The new sigma
  -- 2. bool for when sigma is ready
  -- 3. new number of ones from the input
  -- 4. new value of the main counter
  -- 5. The accumulator output
  -- 6. Accumulator ready going to the filter
  ( ( BitVector (sigmaWidth + 1),
      Bool,
      BitVector inputWidth,
      Unsigned (sigmaWidth - inputWidth + 1)
    ),
    ( BitVector sigmaWidth,
      Bool
    )
  )
accumulatorCounter (sigma, rollover, numOnes, count) numOnes' =
  ((newState, rollover', numOnes', count'), (resize sigma, rollover))
  where
    count' = count + 1
    rollover' = (count == 0)
    newState
      | rollover = resize numOnes
      | sigma /= maxBound = min (sigma + resize numOnes) (shiftR maxBound 1)
      | otherwise = maxBound

-- | Box avaraging filter
-- | All functions that start with filter are used for the box averaging filter
filter ::
  forall dom adcWidth filterDepth .
  (HiddenClockResetEnable dom, KnownNat adcWidth, KnownNat filterDepth) =>
  -- | Filter Depth
  SNat filterDepth ->
  -- | The input of the filter, width is equal to the adc width
  Signal dom (BitVector adcWidth) ->
  -- | Boolean to show when the accumulator is ready
  Signal dom Bool ->
  -- | Parts of the tuple
  -- 1. The filtered output
  -- 2. trigger when a new sample is ready
  ( Signal dom (BitVector adcWidth),
    Signal dom Bool
  )
filter _ dataIn accumRdy = (dataOut, resultValid)
  where
    count = regEn (0 :: Unsigned filterDepth) accumRdy (count + 1)
    accum = mealyB filterAccumulator 0 (count, dataIn, accumulate)
    dataOut = mealyB truncateAccum 0 (accum, latchResult)
    accumRdyD1 = register False accumRdy
    accumRdyD2 = register False accumRdyD1
    accumulate = accumRdyD1 .&&. (not <$> accumRdyD2)
    latchResult = accumulate .&&. (count .==. 1)
    resultValid = register False latchResult

-- | The accumulator for the filter, it adds the input to accum when
-- the sample from the accumulator is ready
filterAccumulator ::
  forall filterDepth adcWdith.
  (KnownNat filterDepth, KnownNat adcWidth) =>
  -- | previous count of the filter accumulator
  BitVector (filterDepth + adcWidth) ->
  -- | Parts of the Tuple:
  -- 1. The filter counter
  -- 2. Input data from a filter accumulator
  -- 3. boolean for when the sample from the filter accumulator is ready
  (Unsigned filterDepth, BitVector adcWidth, Bool) ->
  -- | Parts of the Tuple
  -- 1. the new count of the accumulator
  -- 2. the downsampled output of the filter and adc
  (BitVector (filterDepth + adcWidth), BitVector (filterDepth + adcWidth))
filterAccumulator accum (count, dataIn, accumulate)
  | count == 1 && accumulate = (extend dataIn, accum)
  | accumulate = (accum + extend dataIn, accum)
  | otherwise = (accum, accum)

-- | Sigma Delta ADC configurable in the ADC width, OSR and filter depth
--
-- | The frequency for the ADC is equal to
--  clk / 2^((AccumulatorWidth - BitsPerCycle - 1) + FilterDepth),
-- where clk is the operating frequency of the ADC
--
-- The R and C of the low pass network need to be chosen such that:
-- RC inbetween 200 and 1000 x clk, where clk is the frequency of
-- output
--
-- For the design either lvds or an external comparator can be used
-- the input signal had to be connected to the positive input while
-- the output from a low pass RC-network connects to the negative input.
-- The input is a bitvector such that ddr or a deserialiser can be utilised
-- to get more bits per clock cycle for an increased sampling frequency.
sigmaDeltaADC ::
  forall adcWidth accumExtra filterDepth inputWidth dom .
  ( HiddenClockResetEnable dom,
    KnownNat adcWidth,
    KnownNat inputWidth,
    CLog 2 (inputWidth + 2) <= (adcWidth + accumExtra)
  ) =>
  -- | Accumulator width
  -- Has to be larger or equal to the ADC width,
  -- the OSR is equal to 2^(AccumulatorWidth - ADCwidth)
  SNat (accumExtra + adcWidth) ->
  -- | Filter depth
  -- The depth of the decimation filter,
  -- the downsampling rate is equal to 2^FilterDepth
  SNat filterDepth ->
  -- | analog output from the comparator
  Signal dom (BitVector (inputWidth + 1)) ->
  -- | parts of the tuple
  --
  -- 1. feedback to the RC network:
  --
  -- 2. the digital output of the ADC
  --
  -- 3. Trigger for when the output is ready
  ( Signal dom (BitVector (inputWidth + 1)),
    Signal dom (BitVector adcWidth),
    Signal dom Bool
  )
sigmaDeltaADC accumWidth@SNat filterDepth@SNat analogCmp
  = (delta, digitalOut, sampleRdy)
  where
    delta = register 0 analogCmp
    (accum, accumRdy) = accumulator accumWidth numOnes
    (digitalOut, sampleRdy) = filter filterDepth accum accumRdy
    numOnes = pack <$> (popCountBV <$> delta)
