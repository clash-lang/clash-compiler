{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd,
                  2021     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Prelude.Testbench
  ( -- * Testbench functions for circuits
    assert
  , assertBitVector
  , ignoreFor
  , outputVerifier'
  , outputVerifierBitVector'
  , stimuliGenerator

  , E.tbClockGen
  , E.tbEnableGen
  , E.tbSystemClockGen
  , E.seClockToDiffClock
  )
where

import GHC.TypeLits                       (KnownNat, type (<=))

import qualified Clash.Explicit.Testbench as E
import           Clash.Signal
  (HiddenClock, HiddenReset, HiddenClockResetEnable, Signal,
  hideClock, hideReset, hideClockResetEnable)
import Clash.Promoted.Nat                 (SNat)
import Clash.Sized.BitVector              (BitVector)
import Clash.Sized.Vector                 (Vec)
import Clash.XException                   (ShowX)

{- $setup
>>> :set -XTemplateHaskell -XDataKinds -XTypeApplications
>>> :m -Clash.Explicit.Prelude
>>> :m -Clash.Explicit.Prelude.Safe
>>> :m -Clash.Explicit.Testbench
>>> import Clash.Prelude
>>> import Clash.Prelude.Testbench
>>> let testInput = stimuliGenerator $(listToVecTH [(1::Int),3..21])
>>> let expectedOutput = outputVerifier' $(listToVecTH ([70,99,2,3,4,5,7,8,9,10]::[Int]))
-}

-- | Compares the first two 'Signal's for equality and logs a warning when they
-- are not equal. The second 'Signal' is considered the expected value. This
-- function simply returns the third 'Signal' unaltered as its result. This
-- function is used by 'outputVerifier''.
--
-- === Usage in @clashi@ #assert-clashi#
--
-- __NB__: When simulating a component that uses 'assert' in @clashi@, usually,
-- the warnings are only logged the first time the component is simulated.
-- Issuing @:reload@ in @clashi@ will discard the cached result of the
-- computation, and warnings will once again be emitted.
--
-- __NB__: This function /can/ be used in synthesizable designs.
assert
  :: (Eq a, ShowX a, HiddenClock dom , HiddenReset dom )
  => String
  -- ^ Additional message
  -> Signal dom a
  -- ^ Checked value
  -> Signal dom a
  -- ^ Expected value
  -> Signal dom b
  -- ^ Return value
  -> Signal dom b
assert msg actual expected ret =
  hideReset (hideClock E.assert) msg actual expected ret
{-# INLINE assert #-}

-- | The same as 'assert', but can handle don't care bits in its expected value.
assertBitVector
  :: (KnownNat n, HiddenClock dom , HiddenReset dom )
  => String
  -- ^ Additional message
  -> Signal dom (BitVector n)
  -- ^ Checked value
  -> Signal dom (BitVector n)
  -- ^ Expected value
  -> Signal dom b
  -- ^ Return value
  -> Signal dom b
assertBitVector msg actual expected ret =
  hideReset (hideClock E.assertBitVector) msg actual expected ret
{-# INLINE assertBitVector #-}

-- |
--
-- Example:
--
-- @
-- testInput
--   :: HiddenClockResetEnable dom
--   => 'Signal' dom Int
-- testInput = 'stimuliGenerator' $('Clash.Sized.Vector.listToVecTH' [(1::Int),3..21])
-- @
--
-- >>> sampleN @System 13 testInput
-- [1,1,3,5,7,9,11,13,15,17,19,21,21]
stimuliGenerator
  :: ( KnownNat l
     , HiddenClock dom
     , HiddenReset dom  )
  => Vec l a
  -- ^ Samples to generate
  -> Signal dom a
  -- ^ Signal of given samples
stimuliGenerator = hideReset (hideClock E.stimuliGenerator)
{-# INLINE stimuliGenerator #-}

-- | Compare a signal (coming from a circuit) to a vector of samples. If a
-- sample from the signal is not equal to the corresponding sample in the
-- vector, print to stderr and continue testing. This function is
-- synthesizable in the sense that HDL simulators will run it.
--
-- __NB__: This function uses 'assert'. When simulating this function in
-- @clashi@, read the [note](#assert-clashi).
--
-- Example:
--
-- @
-- expectedOutput
--   :: HiddenClockResetEnable dom
--   -> 'Signal' dom Int -> 'Signal' dom Bool
-- expectedOutput = 'outputVerifier'' $('Clash.Sized.Vector.listToVecTH' ([70,99,2,3,4,5,7,8,9,10]::[Int]))
-- @
--
-- >>> import qualified Data.List as List
-- >>> sampleN @System 12 (expectedOutput (fromList (0:[0..10] List.++ [10,10,10])))
-- <BLANKLINE>
-- cycle(<Clock: System>): 0, outputVerifier
-- expected value: 70, not equal to actual value: 0
-- [False
-- cycle(<Clock: System>): 1, outputVerifier
-- expected value: 70, not equal to actual value: 0
-- ,False
-- cycle(<Clock: System>): 2, outputVerifier
-- expected value: 99, not equal to actual value: 1
-- ,False,False,False,False,False
-- cycle(<Clock: System>): 7, outputVerifier
-- expected value: 7, not equal to actual value: 6
-- ,False
-- cycle(<Clock: System>): 8, outputVerifier
-- expected value: 8, not equal to actual value: 7
-- ,False
-- cycle(<Clock: System>): 9, outputVerifier
-- expected value: 9, not equal to actual value: 8
-- ,False
-- cycle(<Clock: System>): 10, outputVerifier
-- expected value: 10, not equal to actual value: 9
-- ,False,True]
--
-- If you're working with 'BitVector's containing don't care bits you should use 'outputVerifierBitVector''.
outputVerifier'
  :: ( KnownNat l
     , Eq a
     , ShowX a
     , HiddenClock dom
     , HiddenReset dom
     , 1 <= l
     )
  => Vec l a
  -- ^ Samples to compare with
  -> Signal dom a
  -- ^ Signal to verify
  -> Signal dom Bool
  -- ^ Indicator that all samples are verified
outputVerifier' = hideReset (hideClock E.outputVerifier')
{-# INLINE outputVerifier' #-}


-- | Same as 'outputVerifier'',
-- but can handle don't care bits in its expected values.
outputVerifierBitVector'
  :: ( KnownNat l
     , KnownNat n
     , HiddenClock dom
     , HiddenReset dom
     , 1 <= l
     )
  => Vec l (BitVector n)
  -- ^ Samples to compare with
  -> Signal dom (BitVector n)
  -- ^ Signal to verify
  -> Signal dom Bool
  -- ^ Indicator that all samples are verified
outputVerifierBitVector' = hideReset (hideClock E.outputVerifierBitVector')
{-# INLINE outputVerifierBitVector' #-}

-- | Ignore signal for a number of cycles, while outputting a static value.
ignoreFor
  :: HiddenClockResetEnable dom
  => SNat n
  -- ^ Number of cycles to ignore incoming signal
  -> a
  -- ^ Value function produces when ignoring signal
  -> Signal dom a
  -- ^ Incoming signal
  -> Signal dom a
  -- ^ Either a passthrough of the incoming signal, or the static value
  -- provided as the second argument.
ignoreFor = hideClockResetEnable E.ignoreFor
{-# INLINE ignoreFor #-}
