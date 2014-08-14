{-# LANGUAGE ScopedTypeVariables #-}

module CLaSH.Prelude.Testbench
  ( -- * Testbench functions for circuits synchronised to the system slock
    sassert
  , stimuliGenerator
  , outputVerifier
    -- * Testbench functions for circuits synchronised to arbitrary clocks
  , csassert
  , cstimuliGenerator
  , coutputVerifier
  )
where

import Control.Applicative   ((<$>), liftA3)
import Debug.Trace           (trace)
import GHC.TypeLits          (KnownNat)
import Prelude               hiding ((!!))

import CLaSH.Signal          (Signal)
import CLaSH.Signal.Explicit (CSignal, SClock, cregister, systemClock)
import CLaSH.Signal.Wrap     (wrap)
import CLaSH.Sized.Index     (Index)
import CLaSH.Sized.Vector    (Vec, (!!), maxIndex)

{-# NOINLINE sassert #-}
-- | Compares the first two arguments for equality and logs a warning when they
-- are not equal. The second argument is considered the expected value. This
-- function simply returns the third argument unaltered as its result. This
-- function is used by 'outputVerifier'.
--
-- This function is translated to the following VHDL:
--
-- > sassert_block : block
-- > begin
-- >   -- pragma translate_off
-- >   process(clk_1000,reset_1000,arg0,arg1) is
-- >   begin
-- >     if (rising_edge(clk_1000) or rising_edge(reset_1000)) then
-- >       assert (arg0 = arg1) report ("expected: " & to_string (arg1) & \", actual: \" & to_string (arg0)) severity error;
-- >     end if;
-- >   end process;
-- >   -- pragma translate_on
-- >   result <= arg2;
-- > end block;
--
-- And can, due to the pragmas, be used in synthesizable designs
sassert :: (Eq a, Show a)
        => Signal a -- ^ Checked value
        -> Signal a -- ^ Expected value
        -> Signal b -- ^ Returned value
        -> Signal b
sassert = csassert

{-# INLINE stimuliGenerator #-}
-- | To be used as a one of the functions to create the \"magical\" 'testInput'
-- value, which the CλaSH compilers looks for to create the stimulus generator
-- for the generated VHDL testbench.
--
-- Example:
--
-- > testInput :: Signal Int
-- > testInput = stimuliGenerator $(v [(1::Int),3..21])
--
-- >>> sample testInput
-- [1,3,5,7,9,11,13,15,17,19,21,21,21,...
stimuliGenerator :: forall l a . KnownNat l
                 => Vec l a  -- ^ Samples to generate
                 -> Signal a -- ^ Signal of given samples
stimuliGenerator = cstimuliGenerator systemClock

{-# INLINE outputVerifier #-}
-- | To be used as a functions to generate the \"magical\" 'expectedOutput'
-- function, which the CλaSH compilers looks for to create the signal verifier
-- for the generated VHDL testbench.
--
-- Example:
--
-- > expectedOutput :: Signal Int -> Signal Bool
-- > expectedOutput = outputVerifier $(v ([70,99,2,3,4,5,7,8,9,10]::[Int]))
--
-- >>> sample (expectedOutput (fromList ([0..10] ++ [10,10,10])))
-- [
-- expected value: 70, not equal to actual value: 0
-- False,
-- expected value: 99, not equal to actual value: 1
-- False,False,False,False,False,
-- expected value: 7, not equal to actual value: 6
-- False,
-- expected value: 8, not equal to actual value: 7
-- False,
-- expected value: 9, not equal to actual value: 8
-- False,
-- expected value: 10, not equal to actual value: 9
-- False,True,True,...
outputVerifier :: forall l a . (KnownNat l, Eq a, Show a)
               => Vec l a     -- ^ Samples to compare with
               -> Signal a    -- ^ Signal to verify
               -> Signal Bool -- ^ Indicator that all samples are verified
outputVerifier = coutputVerifier systemClock

{-# NOINLINE csassert #-}
-- | Compares the first two arguments for equality and logs a warning when they
-- are not equal. The second argument is considered the expected value. This
-- function simply returns the third argument unaltered as its result. This
-- function is used by 'coutputVerifier'.
--
--
-- This function is translated to the following VHDL:
--
-- > csassert_block : block
-- > begin
-- >   -- pragma translate_off
-- >   process(clk_t,reset_t,arg0,arg1) is
-- >   begin
-- >     if (rising_edge(clk_t) or rising_edge(reset_t)) then
-- >       assert (arg0 = arg1) report ("expected: " & to_string (arg1) & \", actual: \" & to_string (arg0)) severity error;
-- >     end if;
-- >   end process;
-- >   -- pragma translate_on
-- >   result <= arg2;
-- > end block;
--
-- And can, due to the pragmas, be used in synthesizable designs
csassert :: (Eq a,Show a)
         => CSignal t a -- ^ Checked value
         -> CSignal t a -- ^ Expected value
         -> CSignal t b -- ^ Return valued
         -> CSignal t b
csassert = liftA3
  (\a' b' c' -> if a' == b' then c'
                            else trace (concat [ "\nexpected value: "
                                               , show b'
                                               , ", not equal to actual value: "
                                               , show a'
                                               ]) c')

{-# INLINABLE cstimuliGenerator #-}
-- | To be used as a one of the functions to create the \"magical\" 'testInput'
-- value, which the CλaSH compilers looks for to create the stimulus generator
-- for the generated VHDL testbench.
--
-- Example:
--
-- > clk2 = Clock d2
-- >
-- > testInput :: CSignal 2 Int
-- > testInput = cstimuliGenerator $(v [(1::Int),3..21]) clk2
--
-- >>> csample testInput
-- [1,3,5,7,9,11,13,15,17,19,21,21,21,...
cstimuliGenerator :: forall l clk a . KnownNat l
                  => SClock clk     -- ^ Clock to which to synchronize the
                                    -- output signal
                  -> Vec l a        -- ^ Samples to generate
                  -> CSignal clk a  -- ^ Signal of given samples
cstimuliGenerator clk samples =
    let (r,o) = wrap clk (genT <$> cregister clk 0 r)
    in  o
  where
    genT :: Index l -> (Index l,a)
    genT s = (s',samples !! s)
      where
        maxI = fromInteger (maxIndex samples)

        s' = if s < maxI
                then s + 1
                else s

{-# INLINABLE coutputVerifier #-}
-- | To be used as a functions to generate the \"magical\" 'expectedOutput'
-- function, which the CλaSH compilers looks for to create the signal verifier
-- for the generated VHDL testbench.
--
-- Example:
--
-- > clk7 = Clock d7
-- >
-- > expectedOutput :: CSignal 7 Int -> CSignal 7 Bool
-- > expectedOutput = coutputVerifier $(v ([70,99,2,3,4,5,7,8,9,10]::[Int])) clk7
--
-- >>> csample (expectedOutput (cfromList ([0..10] ++ [10,10,10])))
-- [
-- expected value: 70, not equal to actual value: 0
-- False,
-- expected value: 99, not equal to actual value: 1
-- False,False,False,False,False,
-- expected value: 7, not equal to actual value: 6
-- False,
-- expected value: 8, not equal to actual value: 7
-- False,
-- expected value: 9, not equal to actual value: 8
-- False,
-- expected value: 10, not equal to actual value: 9
-- False,True,True,...
coutputVerifier :: forall l clk a . (KnownNat l, Eq a, Show a)
                => SClock clk       -- ^ Clock to which the input signal is
                                    -- synchronized to
                -> Vec l a          -- ^ Samples to compare with
                -> CSignal clk a    -- ^ Signal to verify
                -> CSignal clk Bool -- ^ Indicator that all samples are verified
coutputVerifier clk samples i =
    let (s,o) = wrap clk (genT <$> cregister clk 0 s)
        (e,f) = wrap clk o
    in  csassert i e (cregister clk False f)
  where
    genT :: Index l -> (Index l,(a,Bool))
    genT s = (s',(samples !! s,finished))
      where
        maxI = fromInteger (maxIndex samples)

        s' = if s < maxI
                then s + 1
                else s

        finished = s == maxI
