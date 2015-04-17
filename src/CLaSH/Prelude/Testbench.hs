{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module CLaSH.Prelude.Testbench
  ( -- * Testbench functions for circuits synchronised to the system slock
    assert
  , stimuliGenerator
  , outputVerifier
    -- * Testbench functions for circuits synchronised to arbitrary clocks
  , stimuliGenerator'
  , outputVerifier'
  )
where

import Control.Applicative   ((<$>), liftA3)
import Debug.Trace           (trace)
import GHC.TypeLits          (KnownNat)
import Prelude               hiding ((!!))

import CLaSH.Signal          (Signal)
import CLaSH.Signal.Explicit (Signal', SClock, register', systemClock)
import CLaSH.Signal.Bundle   (unbundle')
import CLaSH.Sized.Index     (Index)
import CLaSH.Sized.Vector    (Vec, (!!), maxIndex)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XDataKinds
-- >>> import CLaSH.Prelude
-- >>> let testInput = stimuliGenerator $(v [(1::Int),3..21])
-- >>> let expectedOutput = outputVerifier $(v ([70,99,2,3,4,5,7,8,9,10]::[Int]))
-- >>> import CLaSH.Prelude.Explicit
-- >>> type ClkA = Clk "A" 100
-- >>> let clkA = sclock :: SClock ClkA
-- >>> let testInput' = stimuliGenerator' clkA $(v [(1::Int),3..21])
-- >>> let expectedOutput' = outputVerifier' clkA $(v ([70,99,2,3,4,5,7,8,9,10]::[Int]))

{-# INLINE stimuliGenerator #-}
-- | To be used as a one of the functions to create the \"magical\" 'testInput'
-- value, which the CλaSH compilers looks for to create the stimulus generator
-- for the generated VHDL testbench.
--
-- Example:
--
-- @
-- testInput :: 'Signal' Int
-- testInput = 'stimuliGenerator' $('CLaSH.Sized.Vector.v' [(1::Int),3..21])
-- @
--
-- >>> sampleN 13 testInput
-- [1,3,5,7,9,11,13,15,17,19,21,21,21]
stimuliGenerator :: forall l a . KnownNat l
                 => Vec l a  -- ^ Samples to generate
                 -> Signal a -- ^ Signal of given samples
stimuliGenerator = stimuliGenerator' systemClock

{-# INLINE outputVerifier #-}
-- | To be used as a functions to generate the \"magical\" 'expectedOutput'
-- function, which the CλaSH compilers looks for to create the signal verifier
-- for the generated VHDL testbench.
--
-- Example:
--
-- @
-- expectedOutput :: 'Signal' Int -> 'Signal' Bool
-- expectedOutput = 'outputVerifier' $('CLaSH.Sized.Vector.v' ([70,99,2,3,4,5,7,8,9,10]::[Int]))
-- @
--
-- >>> import qualified Data.List as List
-- >>> sampleN 12 (expectedOutput (fromList ([0..10] List.++ [10,10,10])))
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
-- False,True,True]
outputVerifier :: forall l a . (KnownNat l, Eq a, Show a)
               => Vec l a     -- ^ Samples to compare with
               -> Signal a    -- ^ Signal to verify
               -> Signal Bool -- ^ Indicator that all samples are verified
outputVerifier = outputVerifier' systemClock

{-# NOINLINE assert #-}
-- | Compares the first two arguments for equality and logs a warning when they
-- are not equal. The second argument is considered the expected value. This
-- function simply returns the third argument unaltered as its result. This
-- function is used by 'coutputVerifier'.
--
--
-- __NB__: This function is /can/ be used in synthesizable designs.
assert :: (Eq a,Show a)
       => Signal' t a -- ^ Checked value
       -> Signal' t a -- ^ Expected value
       -> Signal' t b -- ^ Return valued
       -> Signal' t b
assert = liftA3
  (\a' b' c' -> if a' == b' then c'
                            else trace (concat [ "\nexpected value: "
                                               , show b'
                                               , ", not equal to actual value: "
                                               , show a'
                                               ]) c')

{-# INLINABLE stimuliGenerator' #-}
-- | To be used as a one of the functions to create the \"magical\" 'testInput'
-- value, which the CλaSH compilers looks for to create the stimulus generator
-- for the generated VHDL testbench.
--
-- Example:
--
-- @
-- type ClkA = 'CLaSH.Signal.Explicit.Clk' \"A\" 100
--
-- clkA :: 'SClock' ClkA
-- clkA = 'CLaSH.Signal.Explicit.sclock'
--
-- testInput' :: 'Signal'' clkA Int
-- testInput' = 'stimuliGenerator'' clkA $('CLaSH.Sized.Vector.v' [(1::Int),3..21])
-- @
--
-- >>> sampleN 13 testInput'
-- [1,3,5,7,9,11,13,15,17,19,21,21,21]
stimuliGenerator' :: forall l clk a . KnownNat l
                  => SClock clk     -- ^ Clock to which to synchronize the
                                    -- output signal
                  -> Vec l a        -- ^ Samples to generate
                  -> Signal' clk a  -- ^ Signal of given samples
stimuliGenerator' clk samples =
    let (r,o) = unbundle' clk (genT <$> register' clk 0 r)
    in  o
  where
    genT :: Index l -> (Index l,a)
    genT s = (s',samples !! s)
      where
        maxI = fromInteger (maxIndex samples)

        s' = if s < maxI
                then s + 1
                else s

{-# INLINABLE outputVerifier' #-}
-- | To be used as a functions to generate the \"magical\" 'expectedOutput'
-- function, which the CλaSH compilers looks for to create the signal verifier
-- for the generated VHDL testbench.
--
-- Example:
--
-- @
-- type ClkA = 'CLaSH.Signal.Explicit.Clk' \"A\" 100
--
-- clkA :: 'SClock' ClkA
-- clkA = 'CLaSH.Signal.Explicit.sclock'
--
-- expectedOutput' :: 'Signal'' ClkA Int -> 'Signal'' ClkA Bool
-- expectedOutput' = 'outputVerifier'' clkA $('CLaSH.Sized.Vector.v' ([70,99,2,3,4,5,7,8,9,10]::[Int]))
-- @
--
-- >>> import qualified Data.List as List
-- >>> sampleN 12 (expectedOutput' (fromList ([0..10] List.++ [10,10,10])))
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
-- False,True,True]
outputVerifier' :: forall l clk a . (KnownNat l, Eq a, Show a)
                => SClock clk       -- ^ Clock to which the input signal is
                                    -- synchronized to
                -> Vec l a          -- ^ Samples to compare with
                -> Signal' clk a    -- ^ Signal to verify
                -> Signal' clk Bool -- ^ Indicator that all samples are verified
outputVerifier' clk samples i =
    let (s,o) = unbundle' clk (genT <$> register' clk 0 s)
        (e,f) = unbundle' clk o
    in  assert i e (register' clk False f)
  where
    genT :: Index l -> (Index l,(a,Bool))
    genT s = (s',(samples !! s,finished))
      where
        maxI = fromInteger (maxIndex samples)

        s' = if s < maxI
                then s + 1
                else s

        finished = s == maxI
