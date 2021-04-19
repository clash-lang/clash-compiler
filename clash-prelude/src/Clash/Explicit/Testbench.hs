{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Explicit.Testbench
  ( -- * Testbench functions for circuits
    assert
  , assertBitVector
  , ignoreFor
  , stimuliGenerator

  , tbClockGen
  , tbEnableGen
  , tbSystemClockGen

  , outputVerifier
  , outputVerifier'
  , outputVerifierBitVector
  , outputVerifierBitVector'
  , biTbClockGen
  )
where

import Control.Exception     (catch, evaluate)
import Debug.Trace           (trace)
import GHC.TypeLits          (KnownNat, type (+))
import Prelude               hiding ((!!), length)
import System.IO.Unsafe      (unsafeDupablePerformIO)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Class.Num       (satSucc, SaturationMode(SatBound))
import Clash.Promoted.Nat    (SNat(..), snatToNum)
import Clash.Promoted.Symbol (SSymbol (..))
import Clash.Explicit.Signal
  (Clock, Reset, System, Signal, clockPeriod, toEnable, fromList, register,
  unbundle, unsafeSynchronizer, veryUnsafeSynchronizer)
import Clash.Signal.Internal (Clock (..), Reset (..))
import Clash.Signal          (mux, KnownDomain, Enable)
import Clash.Sized.Index     (Index)
import Clash.Sized.Internal.BitVector
  (BitVector, isLike#)
import Clash.Sized.Vector    (Vec, (!!), length)
import Clash.XException      (ShowX (..), XException)

-- Note that outputVerifier' is used in $setup, while the examples mention
-- outputVerifier. This is fine, as the examples have explicit type
-- signatures, turning 'outputVerifier' into 'outputVerifier''.

{- $setup
>>> :set -XTemplateHaskell -XDataKinds -XTypeFamilies
>>> :m -Clash.Prelude
>>> :m -Clash.Prelude.Safe
>>> :m -Clash.Prelude.Testbench
>>> import Clash.Explicit.Prelude
>>> let testInput clk rst = stimuliGenerator clk rst $(listToVecTH [(1::Int),3..21])
>>> let expectedOutput clk rst = outputVerifier' clk rst $(listToVecTH ([70,99,2,3,4,5,7,8,9,10]::[Int]))
-}

-- | Compares the first two 'Signal's for equality and logs a warning when they
-- are not equal. The second 'Signal' is considered the expected value. This
-- function simply returns the third 'Signal' unaltered as its result. This
-- function is used by 'outputVerifier'.
--
--
-- __NB__: This function /can/ be used in synthesizable designs.
assert
  :: (KnownDomain dom, Eq a, ShowX a)
  => Clock dom
  -> Reset dom
  -> String
  -- ^ Additional message
  -> Signal dom a
  -- ^ Checked value
  -> Signal dom a
  -- ^ Expected value
  -> Signal dom b
  -- ^ Return value
  -> Signal dom b
assert clk (Reset _) msg checked expected returned =
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
{-# ANN assert hasBlackBox #-}

-- | The same as 'assert', but can handle don't care bits in it's expected value.
assertBitVector
  :: (KnownDomain dom, KnownNat n)
  => Clock dom
  -> Reset dom
  -> String
  -- ^ Additional message
  -> Signal dom (BitVector n)
  -- ^ Checked value
  -> Signal dom (BitVector n)
  -- ^ Expected value
  -> Signal dom b
  -- ^ Return value
  -> Signal dom b
assertBitVector clk (Reset _) msg checked expected returned =
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
    eqX a b = unsafeDupablePerformIO (catch (evaluate (a `isLike#` b))
                                            (\(_ :: XException) -> return False))
{-# NOINLINE assertBitVector #-}
{-# ANN assertBitVector hasBlackBox #-}



-- |
--
-- Example:
--
-- @
-- testInput
--   :: KnownDomain dom
--   => Clock dom
--   -> Reset dom
--   -> 'Signal' dom Int
-- testInput clk rst = 'stimuliGenerator' clk rst $('Clash.Sized.Vector.listToVecTH' [(1::Int),3..21])
-- @
--
-- >>> sampleN 14 (testInput systemClockGen resetGen)
-- [1,1,3,5,7,9,11,13,15,17,19,21,21,21]
stimuliGenerator
  :: forall l dom   a
   . ( KnownNat l
     , KnownDomain dom )
  => Clock dom
  -- ^ Clock to which to synchronize the output signal
  -> Reset dom
  -> Vec l a
  -- ^ Samples to generate
  -> Signal dom a
  -- ^ Signal of given samples
stimuliGenerator clk rst samples =
    let (r,o) = unbundle (genT <$> register clk rst (toEnable (pure True)) 0 r)
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

-- | Same as 'outputVerifier' but used in cases where the testbench domain and
-- the domain of the circuit under test are the same.
outputVerifier'
  :: forall l a dom
   . ( KnownNat l
     , KnownDomain dom
     , Eq a
     , ShowX a )
  => Clock dom
  -- ^ Clock to which the testbench is synchronized to
  -> Reset dom
  -- ^ Reset line of testbench
  -> Vec l a
  -- ^ Samples to compare with
  -> Signal dom a
  -- ^ Signal to verify
  -> Signal dom Bool
  -- ^ Indicator that all samples are verified
outputVerifier' =
  outputVerifier @l @a @dom @dom
{-# INLINABLE outputVerifier' #-}

-- | Compare a signal (coming from a circuit) to a vector of samples. If a
-- sample from the signal is not equal to the corresponding sample in the
-- vector, print to stderr and continue testing. This function is
-- synthesizable in the sense that HDL simulators will run it.
--
-- Example:
--
-- @
-- expectedOutput
--   :: Clock dom -> Reset dom
--   -> 'Signal' dom Int -> 'Signal' dom Bool
-- expectedOutput clk rst = 'outputVerifier' clk rst $('Clash.Sized.Vector.listToVecTH' ([70,99,2,3,4,5,7,8,9,10]::[Int]))
-- @
--
-- >>> import qualified Data.List as List
-- >>> sampleN 12 (expectedOutput systemClockGen resetGen (fromList (0:[0..10] List.++ [10,10,10])))
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
-- If your working with 'BitVector's containing don't care bits you should
-- use 'outputVerifierBitVector'.
outputVerifier
  :: forall l a testDom circuitDom
   . ( KnownNat l
     , KnownDomain testDom
     , KnownDomain circuitDom
     , Eq a
     , ShowX a )
  => Clock testDom
  -- ^ Clock to which the testbench is synchronized to (but not necessarily
  -- the circuit under test)
  -> Reset testDom
  -- ^ Reset line of testbench
  -> Vec l a
  -- ^ Samples to compare with
  -> Signal circuitDom a
  -- ^ Signal to verify
  -> Signal testDom Bool
  -- ^ True if all samples are verified
outputVerifier clk rst samples i0 =
    let t1    = snatToNum (clockPeriod @circuitDom)
        t2    = snatToNum (clockPeriod @testDom)
        i1    = veryUnsafeSynchronizer t1 t2 i0
        en    = toEnable (pure True)
        (s,o) = unbundle (genT <$> register clk rst en 0 s)
        (e,f) = unbundle o
        f'    = register clk rst en False f
        -- Only assert while not finished
    in  mux f' f' $ assert clk rst "outputVerifier" i1 e f'
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

-- | Same as 'outputVerifier'', but can handle don't care bits in it's expected
-- values.
outputVerifierBitVector'
  :: forall l n dom
   . ( KnownNat l
     , KnownNat n
     , KnownDomain dom
     )
  => Clock dom
  -- ^ Clock to which the input signal is synchronized to
  -> Reset dom
  -> Vec l (BitVector n)
  -- ^ Samples to compare with
  -> Signal dom (BitVector n)
  -- ^ Signal to verify
  -> Signal dom Bool
  -- ^ Indicator that all samples are verified
outputVerifierBitVector' =
  outputVerifierBitVector @l @n @dom @dom
{-# INLINABLE outputVerifierBitVector' #-}

-- | Same as 'outputVerifier', but can handle don't care bits in it's
-- expected values.
outputVerifierBitVector
  :: forall l n testDom circuitDom
   . ( KnownNat l
     , KnownNat n
     , KnownDomain testDom
     , KnownDomain circuitDom
     )
  => Clock testDom
  -- ^ Clock to which the input signal is synchronized to
  -> Reset testDom
  -> Vec l (BitVector n)
  -- ^ Samples to compare with
  -> Signal circuitDom (BitVector n)
  -- ^ Signal to verify
  -> Signal testDom Bool
  -- ^ Indicator that all samples are verified
outputVerifierBitVector clk rst samples i0 =
    let t1    = snatToNum (clockPeriod @circuitDom)
        t2    = snatToNum (clockPeriod @testDom)
        i1    = veryUnsafeSynchronizer t1 t2 i0
        en    = toEnable (pure True)
        (s,o) = unbundle (genT <$> register clk rst en 0 s)
        (e,f) = unbundle o
        f'    = register clk rst en False f
        -- Only assert while not finished
    in  mux f' f' $ assertBitVector clk rst "outputVerifierBitVector'" i1 e f'
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
  :: forall dom  n a
   . KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> SNat n
  -- ^ Number of cycles to ignore incoming signal
  -> a
  -- ^ Value function produces when ignoring signal
  -> Signal dom a
  -- ^ Incoming signal
  -> Signal dom a
  -- ^ Either a passthrough of the incoming signal, or the static value
  -- provided as the second argument.
ignoreFor clk rst en SNat a i =
  mux ((==) <$> counter <*> (pure maxBound)) i (pure a)
 where
  counter :: Signal dom (Index (n+1))
  counter = register clk rst en 0 (satSucc SatBound <$> counter)

-- | Same as 'tbClockGen', but returns two clocks on potentially different
-- domains. To be used in situations where the circuit under test runs
-- in a different domain than the circuit testing it. Most commonly used
-- to test synchronous circuits (with an asynchronous test circuit).
biTbClockGen
  :: forall testDom circuitDom
   . ( KnownDomain testDom
     , KnownDomain circuitDom
     )
  => Signal testDom Bool
  -> (Clock testDom, Clock circuitDom)
biTbClockGen done = (testClk, circuitClk)
 where
  testClk = tbClockGen done
  circuitClk = tbClockGen (unsafeSynchronizer testClk circuitClk done)

-- | Clock generator to be used in the /testBench/ function.
--
-- To be used like:
--
-- @
-- clkSystem en = tbClockGen @System en
-- @
--
-- === __Example__
--
-- @
-- module Example where
--
-- import "Clash.Explicit.Prelude"
-- import "Clash.Explicit.Testbench"
--
-- -- Fast domain: twice as fast as \"Slow\"
-- 'Clash.Explicit.Prelude.createDomain' 'Clash.Explicit.Prelude.vSystem'{vName=\"Fast\", vPeriod=10}
--
-- -- Slow domain: twice as slow as \"Fast\"
-- 'Clash.Explicit.Prelude.createDomain' 'Clash.Explicit.Prelude.vSystem'{vName=\"Slow\", vPeriod=20}
--
-- topEntity
--   :: 'Clock' \"Fast\"
--   -> 'Reset' \"Fast\"
--   -> 'Enable' \"Fast\"
--   -> 'Clock' \"Slow\"
--   -> 'Signal' \"Fast\" (Unsigned 8)
--   -> 'Signal' \"Slow\" (Unsigned 8, Unsigned 8)
-- topEntity clk1 rst1 en1 clk2 i =
--   let h = register clk1 rst1 en1 0 (register clk1 rst1 en1 0 i)
--       l = register clk1 rst1 en1 0 i
--   in  unsafeSynchronizer clk1 clk2 (bundle (h, l))
--
-- testBench
--   :: 'Signal' \"Slow\" Bool
-- testBench = done
--   where
--     testInput      = 'Clash.Explicit.Testbench.stimuliGenerator' clkA1 rstA1 $('Clash.Sized.Vector.listToVecTH' [1::Unsigned 8,2,3,4,5,6,7,8])
--     expectedOutput = 'Clash.Explicit.Testbench.outputVerifier'   clkB2 rstB2 $('Clash.Sized.Vector.listToVecTH' [(0,0) :: (Unsigned 8, Unsigned 8),(1,2),(3,4),(5,6),(7,8)])
--     done           = expectedOutput (topEntity clkA1 rstA1 enableGen clkB2 testInput)
--     done'          = not \<$\> done
--     clkA1          = 'tbClockGen' \@\"Fast\" (unsafeSynchronizer clkB2 clkA1 done')
--     clkB2          = 'tbClockGen' \@\"Slow\" done'
--     rstA1          = 'Clash.Signal.resetGen' \@\"Fast\"
--     rstB2          = 'Clash.Signal.resetGen' \@\"Slow\"
-- @
tbClockGen
  :: KnownDomain testDom
  => Signal testDom Bool
  -> Clock testDom
tbClockGen done = Clock (done `seq` SSymbol)
{-# NOINLINE tbClockGen #-}
{-# ANN tbClockGen hasBlackBox #-}

-- | Enable signal that's always enabled. Because it has a blackbox definition
-- this enable signal is opaque to other blackboxes. It will therefore never
-- be optimized away.
tbEnableGen :: Enable tag
tbEnableGen = toEnable (pure True)
{-# NOINLINE tbEnableGen #-}
{-# ANN tbEnableGen hasBlackBox #-}

-- | Clock generator for the 'System' clock domain.
--
-- __NB__: can be used in the /testBench/ function
--
-- === __Example__
--
-- @
-- topEntity :: Vec 2 (Vec 3 (Unsigned 8)) -> Vec 6 (Unsigned 8)
-- topEntity = concat
--
-- testBench :: Signal System Bool
-- testBench = done
--   where
--     testInput      = pure ((1 :> 2 :> 3 :> Nil) :> (4 :> 5 :> 6 :> Nil) :> Nil)
--     expectedOutput = outputVerifier ((1:>2:>3:>4:>5:>6:>Nil):>Nil)
--     done           = exposeClockResetEnable (expectedOutput (topEntity <$> testInput)) clk rst
--     clk            = 'tbSystemClockGen' (not <\$\> done)
--     rst            = systemResetGen
-- @
tbSystemClockGen
  :: Signal System Bool
  -> Clock System
tbSystemClockGen = tbClockGen
