{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Explicit.Testbench
  ( -- * Testbench functions for circuits
    assert
  , ignoreFor
  , outputVerifier
  , outputVerifierBitVector
  , stimuliGenerator
  )
where

import Control.Exception     (catch, evaluate)
import Debug.Trace           (trace)
import GHC.TypeLits          (KnownNat, type (+))
import Prelude               hiding ((!!), length)
import System.IO.Unsafe      (unsafeDupablePerformIO)

import Clash.Class.Num       (satAdd, SaturationMode(SatBound))
import Clash.Explicit.Signal
  (Clock, Reset, Signal, fromList, register, unbundle)
import Clash.Promoted.Nat    (SNat(..))
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
         else trace (concat [ "\ncycle(" ++ show clk ++ "): "
                            , show cnt
                            , ", "
                            , msg
                            , "\nexpected value: "
                            , showX e
                            , ", not equal to actual value: "
                            , showX c
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
         else trace (concat [ "\ncycle(" ++ show clk ++ "): "
                            , show cnt
                            , ", "
                            , msg
                            , "\nexpected value: "
                            , showX e
                            , ", not equal to actual value: "
                            , showX c
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
-- cycle(system10000): 0, outputVerifier
-- expected value: 70, not equal to actual value: 0
-- [False
-- cycle(system10000): 1, outputVerifier
-- expected value: 70, not equal to actual value: 0
-- ,False
-- cycle(system10000): 2, outputVerifier
-- expected value: 99, not equal to actual value: 1
-- ,False,False,False,False,False
-- cycle(system10000): 7, outputVerifier
-- expected value: 7, not equal to actual value: 6
-- ,False
-- cycle(system10000): 8, outputVerifier
-- expected value: 8, not equal to actual value: 7
-- ,False
-- cycle(system10000): 9, outputVerifier
-- expected value: 9, not equal to actual value: 8
-- ,False
-- cycle(system10000): 10, outputVerifier
-- expected value: 10, not equal to actual value: 9
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

-- | Ignore signal for a number of cycles, while outputting a static value.
ignoreFor
  :: forall dom gated sync n a
   . Clock dom gated
  -> Reset dom sync
  -> SNat n
  -- ^ Number of cycles to ignore incoming signal
  -> a
  -- ^ Value function produces when ignoring signal
  -> Signal dom a
  -- ^ Incoming signal
  -> Signal dom a
  -- ^ Either a passthrough of the incoming signal, or the static value
  -- provided as the second argument.
ignoreFor clk rst SNat a i =
  mux ((==) <$> counter <*> (pure maxBound)) i (pure a)
 where
  counter :: Signal dom (Index (n+1))
  counter = register clk rst 0 (satAdd SatBound 1 <$> counter)
