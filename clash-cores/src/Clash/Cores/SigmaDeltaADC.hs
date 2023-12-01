module SigmaDeltaADC (sigmaDeltaADC) where

import Clash.Prelude hiding (filter, truncate)

-- | Sigma Delta ADC configurable in the ADC width, OSR and filter depth

-- | The frequency for the ADC is equal to clk / 2^((AccumulatorWidth - BitsPerCycle - 1) + FilterDepth),
-- where clk is the operating frequency of the ADC
{-# NOINLINE sigmaDeltaADC #-}
sigmaDeltaADC ::
  ( HiddenClockResetEnable dom,
    KnownNat n0,
    KnownNat n1,
    KnownNat n2,
    KnownNat n3,
    CLog 2 (n3 + 1) <= (n1 + n0)
  ) =>
  -- | ADC width
  SNat n0 ->
  -- | Accumulator width
  -- Has to be larger or equal to the ADC width, the OSR is equal to 2^(AccumulatorWidth - ADCwidth)
  SNat (n0 + n1) ->
  -- | Filter depth
  -- The depth of the decimation filter, the downsampling rate is equal to 2^FilterDepth
  SNat n2 ->
  -- | analog input from the comparator
  -- For the design either lvds or an external comparator can be used
  -- the input signal had to be connected to the positive input while
  -- the output from a low pass RC-network connects to the negative input.
  -- The input is a bitvector such that ddr or a deserialiser can be utilised
  -- to get more bits per clock cycle for an increased sampling frequency.
  Signal dom (BitVector n3) ->
  -- | parts of the tuple
  --
  -- 1. feedback to the RC network:
  -- The R and C of the low pass network need to be chosen such that:
  -- RC inbetween 200 and 1000 x clk, where clk is the frequency of
  -- output
  --
  -- 2. the digital output of the ADC
  --
  -- 3. Trigger for when the output is ready
  Signal
    dom
    ( BitVector n3,
      BitVector n0,
      Bool
    )
sigmaDeltaADC adcw accumw filterw analog_cmp = bundle (delta, digital_out, sample_rdy)
  where
    delta = register 0 analog_cmp
    (accum, accum_rdy) = unbundle (accumulator adcw accumw numOnes)
    (digital_out, sample_rdy) = unbundle (filter filterw accum accum_rdy)
    numOnes = pack <$> (countOnes <$> delta)

-- | Shared functions
-- Function to remove the LSBs from a BitVector
truncate ::
  (KnownNat n0, KnownNat n1) =>
  -- | entire bitvector
  BitVector (n0 + n1) ->
  -- | bitvector without number of LSBs
  BitVector n0
truncate input = output
  where
    (output, _) = split input

-- | Remove the LSBs from the accumulator output when the accumulator is ready
-- This is used for both the accumulator and the filter
truncateAccum ::
  (KnownNat n0, KnownNat n1) =>
  BitVector n0 ->
  -- The previous accum

  -- | parts of the tuple
  --
  -- 1. Accumulator output
  -- 2. Boolean for when the accumulator is ready
  (BitVector (n0 + n1), Bool) ->
  -- | parts of tuple
  --
  -- 1. updated accum
  -- 2. output accum
  (BitVector n0, BitVector n0)
truncateAccum accum (sigma, rollover)
  | rollover = (sigma', accum)
  | otherwise = (accum, accum)
  where
    sigma' = truncate sigma

-- | Function to do a popcount from the input bit vector
countOnes ::
  (KnownNat n0) =>
-- | The bitvector to do a popcount on
  BitVector n0 ->
-- | The result as an index
  Index (n0 + 1)
countOnes analog_cmp = fromInteger (toInteger (popCount analog_cmp))

-- |Accumulator
-- The main function for the accumulator, all functions that start with accumulator are used for the accumulator
{-# NOINLINE accumulator #-}
accumulator ::
  (HiddenClockResetEnable dom, KnownNat n0, KnownNat n1, KnownNat n2, n2 <= (n1 + n0)) =>
  -- | The ADC width
  SNat n0 ->
  -- | The accumulator width
  SNat (n1 + n0) ->
  -- | The amount of ones from the input
  Signal dom (BitVector n2) ->
  -- | Parts of the Tuple
  --
  -- 1. The accumulator output
  -- 2. Accumulator ready going to the filter
  Signal dom (BitVector n0, Bool)
accumulator _ aw numOnes = bundle (accum, rollover)
  where
    accum = mealyB truncateAccum 0 (sigma, rollover)
    (sigma, rollover) = unbundle (accumulatorCounterT aw numOnes)

accumulatorCounterT ::
  (HiddenClockResetEnable dom, KnownNat n0, KnownNat n1, n1 <= n0) =>
  -- | the accumulator width
  SNat n0 ->
  -- | The amount of ones from the input
  Signal dom (BitVector n1) ->
  -- | Parts of the Tuple
  --
  -- 1. The accumulator output
  -- 2. Accumulator ready going to the filter
  Signal dom (BitVector n0, Bool)
accumulatorCounterT _ numOnes = mealy accumulatorCounter (0, False, 0, 0) numOnes

-- | The counter for sigma, which counts counts the number of ones from the input and resets when count is equal to zero
accumulatorCounter ::
  (KnownNat n0, KnownNat n1, n1 <= n0) =>
  -- | Parts of the Tuple
  --
  -- 1. The previous sigma
  -- 2. bool for when sigma is ready
  -- 3. previous number of ones from the input
  -- 4. previous value of the main counter
  ( BitVector (n0 + 1),
    Bool,
    BitVector n1,
    BitVector (n0 - n1 + 1)
  ) ->
  -- | number of ones from the input
  BitVector n1 ->
  -- | Parts of the Tuple
  --
  -- 1. The new sigma
  -- 2. bool for when sigma is ready
  -- 3. new number of ones from the input
  -- 4. new value of the main counter
  -- 5. The accumulator output
  -- 6. Accumulator ready going to the filter
  ( ( BitVector (n0 + 1),
      Bool,
      BitVector n1,
      BitVector (n0 - n1 + 1)
    ),
    ( BitVector n0,
      Bool
    )
  )
accumulatorCounter (sigma, rollover, num_ones, count) num_ones' =
  ((new_state, rollover', num_ones', count'), (resize sigma, rollover))
  where
    count' = count + 1
    rollover' = (count == 0)

    new_state
      | rollover = resize num_ones
      | sigma /= maxBound = min (sigma + resize num_ones) (shiftR maxBound 1)
      | otherwise = maxBound

-- | Box avaraging filter
-- | All functions that start with filter are used for the box averaging filter
{-# NOINLINE filter #-}
filter ::
  (HiddenClockResetEnable dom, KnownNat n0, KnownNat n1) =>
  -- | Filter Depth
  SNat n1 ->
  -- | The input of the filter, width is equal to the adc width
  Signal dom (BitVector n0) ->
  -- | Boolean to show when the accumulator is ready
  Signal dom Bool ->
  -- | Parts of the tuple
  -- 1. The filtered output
  -- 2. trigger when a new sample is ready
  Signal dom (BitVector n0, Bool)
filter fw data_in accumRdy = bundle (data_out, result_valid)
  where
    (accumulate, latch_result, result_valid) = mealyB filterPipeline (False, False, False) (accumRdy, count)
    count = filterCount fw accumRdy
    accum = mealyB filterAccumulator 0 (count, data_in, accumulate)
    data_out = mealyB truncateAccum 0 (accum, latch_result)

-- | The accumulator for the filter, it adds the input to accum when the sample from the accumulator is ready
filterAccumulator ::
  (KnownNat n0, KnownNat n1) =>
  -- | previous count of the filter accumulator
  BitVector (n0 + n1) ->
  -- | Parts of the Tuple:
  -- 1. The filter counter
  -- 2. Input data from a filter accumulator
  -- 3. boolean for when the sample from the filter accumulator is ready
  (BitVector n1, BitVector n0, Bool) ->
  -- | Parts of the Tuple
  -- 1. the new count of the accumulator
  -- 2. the downsampled output of the filter and adc
  (BitVector (n0 + n1), BitVector (n0 + n1))
filterAccumulator accum (count, data_in, accumulate)
  | count == 1 && accumulate = (extend data_in, accum)
  | accumulate = (accum + extend data_in, accum)
  | otherwise = (accum, accum)

-- | FilterPipeline to sync all the signals
-- contains 3 registers to determine when the filter needs to accumulate.
-- this happens when the accumulator is ready
filterPipeline ::
  (KnownNat n0) =>
  -- | parts of the tuple
  --
  -- 1. accum ready with 1 cycle of delay
  -- 2. accum ready with 2 cycles of delay
  -- 3. latch result with 1 cycle of delay
  (Bool, Bool, Bool) ->
  -- | parts of tuple
  -- input of the mealy machine
  -- 1. accum ready
  -- 2. the main counter
  (Bool, BitVector n0) ->
  -- | parts of the tuple
  --
  -- 1,2,3. input for all the registers
  -- 4, boolean for when the filter need to accumulate
  -- 5. boolean for when the filter accumulator of ready
  -- 6. boolean for when the output for the adc is ready
  ((Bool, Bool, Bool), (Bool, Bool, Bool))
filterPipeline (accumRdy_d1, accumRdy_d2, latch_result) (accumRdy, count) =
  ((accumRdy_d1', accumRdy_d2', latch_result'), (accumulate, latch_result', latch_result))
  where
    accumRdy_d1' = accumRdy
    accumRdy_d2' = accumRdy_d1
    accumulate = accumRdy_d1 && not accumRdy_d2
    latch_result' = accumulate && (count == 1)

-- | Count when the input sample is ready it counts till 2^(filterWidth)
filterCount ::
  (HiddenClockResetEnable dom, KnownNat n0) =>
  SNat n0 ->
  -- | Filter width
  Signal dom Bool ->
  -- | Sample ready boolean
  Signal dom (BitVector n0)
  -- | Counter output
filterCount _ accumRdy = output
  where
    output = regEn 0 accumRdy (output + 1)
