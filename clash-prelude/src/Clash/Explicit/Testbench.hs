{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.Testbench
  ( -- * Testbench functions for circuits
    assert
  , stimuliGenerator
  , outputVerifier
  , outputVerifierBitVector
  )
where

import Control.Exception     (catch, evaluate)
import Debug.Trace           (trace)
import GHC.TypeLits          (KnownNat)
import Prelude               hiding ((!!), length)
import System.IO.Unsafe      (unsafeDupablePerformIO)

import Clash.Explicit.Signal
  (Clock, Reset, Signal, fromList, register, unbundle)
import Clash.Signal          (mux)
import Clash.Sized.Index     (Index)
import Clash.Sized.Internal.BitVector
  (BitVector, isLike)
import Clash.Sized.Vector    (Vec, (!!), length)
import Clash.XException      (ShowX (..), XException)

{- $setup
>>> :set -XTemplateHaskell -XDataKinds
>>> import Clash.Explicit.Prelude
>>> let testInput clk rst = stimuliGenerator clk rst $(listToVecTH [(1::Int),3..21])
>>> let expectedOutput clk rst = outputVerifier clk rst $(listToVecTH ([70,99,2,3,4,5,7,8,9,10]::[Int]))
-}

-- | Compares the first two 'Signal's for equality and logs a warning when they
-- are not equal. The second 'Signal' is considered the expected value. This
-- function simply returns the third 'Signal'' unaltered as its result. This
-- function is used by 'outputVerifier'.
--
--
-- __NB__: This function /can/ be used in synthesizable designs.
assert
  :: (Eq a,ShowX a)
  => Clock domain gated
  -> Reset domain synchronous
  -> String          -- ^ Additional message
  -> Signal domain a -- ^ Checked value
  -> Signal domain a -- ^ Expected value
  -> Signal domain b -- ^ Return value
  -> Signal domain b
assert clk _rst msg checked expected returned =
  (\c e cnt r ->
      if eqX c e
         then r
         else trace (concat [ "\nOn cycle ", show cnt, " of clock '", show clk
                            , "', ", msg, " encountered an unexpected value:\n"
                            , "  Expected: ", showX e, "\n"
                            , "  Actual:   ", showX c
                            ]) r)
  <$> checked <*> expected <*> fromList [(0::Integer)..] <*> returned
  where
    eqX a b = unsafeDupablePerformIO (catch (evaluate (a == b))
                                            (\(_ :: XException) -> return False))
{-# NOINLINE assert #-}

-- | The same as 'assert', but can handle don't care bits in it's expected value.
assertBitVector
  :: (KnownNat n)
  => Clock domain gated
  -> Reset domain synchronous
  -> String                      -- ^ Additional message
  -> Signal domain (BitVector n) -- ^ Checked value
  -> Signal domain (BitVector n) -- ^ Expected value
  -> Signal domain b             -- ^ Return value
  -> Signal domain b
assertBitVector clk _rst msg checked expected returned =
  (\c e cnt r ->
      if eqX c e
         then r
         else trace (concat [ "\nOn cycle ", show cnt, " of ", show clk, ", "
                            , msg, " encountered an unexpected value:\n"
                            , "  Expected: ", showX e, "\n"
                            , "  Actual:   ", showX c
                            ]) r)
  <$> checked <*> expected <*> fromList [(0::Integer)..] <*> returned
  where
    eqX a b = unsafeDupablePerformIO (catch (evaluate (a `isLike` b))
                                            (\(_ :: XException) -> return False))
{-# NOINLINE assertBitVector #-}



-- | To be used as one of the functions to create the \"magical\" 'testInput'
-- value, which the Clash compiler looks for to create the stimulus generator
-- for the generated VHDL testbench.
--
-- Example:
--
-- @
-- testInput
--   :: Clock domain gated -> Reset domain synchronous
--   -> 'Signal' domain Int
-- testInput clk rst = 'stimuliGenerator' clk rst $('Clash.Sized.Vector.listToVecTH' [(1::Int),3..21])
-- @
--
-- >>> sampleN 14 (testInput systemClockGen asyncResetGen)
-- [1,1,3,5,7,9,11,13,15,17,19,21,21,21]
stimuliGenerator
  :: forall l domain gated synchronous a
   . KnownNat l
  => Clock domain gated
  -- ^ Clock to which to synchronize the output signal
  -> Reset domain synchronous
  -> Vec l a        -- ^ Samples to generate
  -> Signal domain a  -- ^ Signal of given samples
stimuliGenerator clk rst samples =
    let (r,o) = unbundle (genT <$> register clk rst 0 r)
    in  o
  where
    genT :: Index l -> (Index l,a)
    genT s = (s',samples !! s)
      where
        maxI = toEnum (length samples - 1)

        s' = if s < maxI
                then s + 1
                else s
{-# INLINABLE stimuliGenerator #-}

-- | To be used as one of the functions to generate the \"magical\" 'expectedOutput'
-- function, which the Clash compiler looks for to create the signal verifier
-- for the generated VHDL testbench.
--
-- Example:
--
-- @
-- expectedOutput
--   :: Clock domain gated -> Reset domain synchronous
--   -> 'Signal' domain Int -> 'Signal' domain Bool
-- expectedOutput clk rst = 'outputVerifier' clk rst $('Clash.Sized.Vector.listToVecTH' ([70,99,2,3,4,5,7,8,9,10]::[Int]))
-- @
--
-- >>> import qualified Data.List as List
-- >>> sampleN 12 (expectedOutput systemClockGen asyncResetGen (fromList (0:[0..10] List.++ [10,10,10])))
-- <BLANKLINE>
-- On cycle 0 of clock 'system10000', outputVerifier encountered an unexpected value:
--   Expected: 70
--   Actual:   0
-- [False
-- On cycle 1 of clock 'system10000', outputVerifier encountered an unexpected value:
--   Expected: 70
--   Actual:   0
-- ,False
-- On cycle 2 of clock 'system10000', outputVerifier encountered an unexpected value:
--   Expected: 99
--   Actual:   1
-- ,False,False,False,False,False
-- On cycle 7 of clock 'system10000', outputVerifier encountered an unexpected value:
--   Expected: 7
--   Actual:   6
-- ,False
-- On cycle 8 of clock 'system10000', outputVerifier encountered an unexpected value:
--   Expected: 8
--   Actual:   7
-- ,False
-- On cycle 9 of clock 'system10000', outputVerifier encountered an unexpected value:
--   Expected: 9
--   Actual:   8
-- ,False
-- On cycle 10 of clock 'system10000', outputVerifier encountered an unexpected value:
--   Expected: 10
--   Actual:   9
-- ,False,True]
--
-- If your working with 'BitVector's containing don't care bit you should use 'outputVerifierBitVector'.
outputVerifier
  :: forall l domain gated synchronous a
   . (KnownNat l, Eq a, ShowX a)
  => Clock domain gated
  -- ^ Clock to which the input signal is synchronized to
  -> Reset domain synchronous
  -> Vec l a          -- ^ Samples to compare with
  -> Signal domain a    -- ^ Signal to verify
  -> Signal domain Bool -- ^ Indicator that all samples are verified
outputVerifier clk rst samples i =
    let (s,o) = unbundle (genT <$> register clk rst 0 s)
        (e,f) = unbundle o
        f'    = register clk rst False f
        -- Only assert while not finished
    in  mux f' f' $ assert clk rst "outputVerifier" i e f'
  where
    genT :: Index l -> (Index l,(a,Bool))
    genT s = (s',(samples !! s,finished))
      where
        maxI = toEnum (length samples - 1)

        s' = if s < maxI
                then s + 1
                else s

        finished = s == maxI
{-# INLINABLE outputVerifier #-}

-- | Same as 'outputVerifier',
-- but can handle don't care bits in it's expected values.
outputVerifierBitVector
  :: forall l n domain gated synchronous
   . (KnownNat l, KnownNat n)
  => Clock domain gated
  -- ^ Clock to which the input signal is synchronized to
  -> Reset domain synchronous
  -> Vec l (BitVector n)         -- ^ Samples to compare with
  -> Signal domain (BitVector n) -- ^ Signal to verify
  -> Signal domain Bool          -- ^ Indicator that all samples are verified
outputVerifierBitVector clk rst samples i =
    let (s,o) = unbundle (genT <$> register clk rst 0 s)
        (e,f) = unbundle o
        f'    = register clk rst False f
        -- Only assert while not finished
    in  mux f' f' $ assertBitVector clk rst "outputVerifierBitVector" i e f'
  where
    genT :: Index l -> (Index l,(BitVector n,Bool))
    genT s = (s',(samples !! s,finished))
      where
        maxI = toEnum (length samples - 1)

        s' = if s < maxI
                then s + 1
                else s

        finished = s == maxI
{-# INLINABLE outputVerifierBitVector #-}
