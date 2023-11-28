module Clash.Cores.SigmaDeltaADC where

import Clash.Prelude hiding (filter, truncate)

-- Shared functions
-- Function to remove the LSBs from a BitVector
truncate :: (KnownNat n0, KnownNat n1) 
          => BitVector (n0 + n1) 
          -> BitVector n0 
truncate input = output
    where 
        (output, lsb) = split input

-- Remove the LSBs from sigma when the rollover is high
updateAccum ::(KnownNat n0, KnownNat n1)
            =>  BitVector n0                    -- The previous accum, the state
            -> (BitVector (n0 + n1), Bool)      -- The input, sigma and the rollover boolean
            -> (BitVector n0, BitVector n0)     -- The new accum and the output
updateAccum accum (sigma, rollover)
    | rollover          = (sigma', accum)
    | otherwise         = (accum, accum) 
        where
            sigma'      = truncate sigma

-- Accumulator

--The main function for the accumulator
{-# NOINLINE accumulator #-}
accumulator :: (HiddenClockResetEnable dom,  KnownNat n0, KnownNat n1, KnownNat n2, n2 <= (n1 + n0))
        => SNat n0                              -- The ADC width 
        -> SNat (n1 + n0)                       -- The accumulator width
        -> Signal dom (BitVector n2)            -- The input from the comparator    
        -> Signal dom (BitVector n0, Bool)      -- The accumulator output and the accumulator ready 
accumulator cw aw numOnes = bundle (accum, rollover)
    where 
        accum               = mealyB updateAccum 0 (sigma, rollover)
        (sigma, rollover)   = unbundle (counterT aw numOnes)


        counterT :: (HiddenClockResetEnable dom, KnownNat n0, KnownNat n1, n1 <= n0) => SNat n0 -> Signal dom (BitVector n1) -> Signal dom (BitVector n0, Bool) 
        counterT aw numOnes = mealy counter (0, False, 0, 0) numOnes

        -- The counter for sigma, which counts up when the input from the comparator is high and resets when count is equal to zero
        counter :: (KnownNat n0, KnownNat n1, n1 <= n0) 
                => (BitVector (n0 + 1), Bool, BitVector n1, BitVector (n0 - n1 + 1))                           --The current state of sigma  
                -> BitVector n1                     --The input, and count
                -> ((BitVector (n0 + 1), Bool, BitVector n1, BitVector (n0 - n1 + 1 )), (BitVector n0, Bool))  --The updated sigma and 
        counter (sigma, rollover, num_ones, count) num_ones' = ((new_state, rollover', num_ones', count'), (resize sigma, rollover))
            where
                count' = count + 1 
                rollover' = count == 0 

                new_state
                    | rollover               = resize num_ones
                    | sigma /= maxBound      = min (sigma + resize num_ones) (shiftR maxBound 1)
                    | otherwise              = maxBound





-- Box avaraging filter
{-# NOINLINE filter #-}
filter ::  (HiddenClockResetEnable dom, KnownNat n0, KnownNat n1)  
        =>  SNat n1                             -- The amount of filter bits                           
        ->  Signal dom (BitVector n0)           -- The input of the filter, width is equal to the adc width
        ->  Signal dom Bool                     -- Boolean to show when the accumulator is ready
        ->  Signal dom (BitVector n0, Bool)     -- The filtered output and the sample_rdy
filter fw data_in sample = bundle(data_out, result_valid)
    where
        (accumulate, latch_result, result_valid)    = mealyB pipeline (False, False, False) (sample, count)
        count                                       = traceSignal1 "Filter.count" (count' fw sample)
        accum                                       = traceSignal1 "Filter.Accum" (mealyB accumulatorf 0 (count, traceSignal1 "Filter.Data_in" data_in, traceSignal1 "Filter.Accumulate" accumulate))
        data_out                                    = traceSignal1 "Filter.data_out" (mealyB updateAccum 0 (accum, latch_result))

        --  The accumulator for the filter, it adds the input to accum when teh sample is ready      
        accumulatorf :: (KnownNat n0, KnownNat n1)
                    =>   BitVector (n0 + n1)                        -- state of accum
                    ->  (BitVector n1, BitVector n0, Bool)          -- The main counter input, the input data, boolean for when the sample is ready
                    ->  (BitVector (n0 + n1), BitVector (n0 + n1))  -- new state, the output
        accumulatorf accum (count, data_in, accumulate)
            | count == 1 && accumulate      = (extend data_in, accum) 
            | accumulate                    = (accum + extend data_in, accum)
            | otherwise                     = (accum, accum)


        -- Pipeline to sync all the signals 
        pipeline :: (KnownNat n0)
                    => (Bool, Bool, Bool)                       --                     
                    -> (Bool, BitVector n0)
                    -> ((Bool, Bool, Bool), (Bool, Bool, Bool))
        pipeline (sample_d1, sample_d2, latch_result) (sample, count) = ((sample_d1', sample_d2', latch_result'), (accumulate, latch_result', latch_result))
            where
                sample_d1'      = sample
                sample_d2'      = sample_d1
                accumulate      = sample_d1 && not sample_d2
                latch_result'   = accumulate && (count == 1) 


        -- Count when the input sample is ready 
        count' :: (HiddenClockResetEnable dom, KnownNat n0) 
                => SNat n0                      -- Filter width 
                -> Signal dom Bool              -- Sample ready boolean
                -> Signal dom (BitVector n0)    -- counter output 
        count' fw sample = output
            where
                output = regEn 0 sample (output + 1)

-- Function for the sigma delta ADC 
{-# NOINLINE sigmaDeltaADC #-}
sigmaDeltaADC :: (HiddenClockResetEnable dom, KnownNat n0, KnownNat n1, KnownNat n2, KnownNat n3, CLog 2 (n3 + 1) <= (n1 + n0)) 
            => SNat n0                                  -- ADC width
            -> SNat (n0 + n1)                           -- Accumnulator width
            -> SNat n2                                  -- Filter width
            -> Signal dom (BitVector n3)                        -- analog innput from the comparator
            -> Signal dom (BitVector n3, BitVector n0, Bool)    -- analog output to the RC network, digital out, sample ready
sigmaDeltaADC cw aw fw analog_cmp =  bundle(delta, digital_out, sample_rdy) 
    where 
        delta                       = register 0 analog_cmp
        (accum, accum_rdy)          = unbundle(accumulator cw aw numOnes)
        (digital_out, sample_rdy)   = unbundle(filter fw accum accum_rdy)

        numOnes = traceSignal1 "adc.numOnes" (pack <$> (countOnes <$> delta))

        countOnes :: (KnownNat n0)
                    => BitVector n0
                    -> Index (n0 + 1)
        countOnes analog_cmp = fromInteger (toInteger(popCount analog_cmp))










