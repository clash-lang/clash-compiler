{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -O0 -fno-omit-interface-pragmas #-}

{- |
  This module defines the explicitly clocked counterparts of the functions
  defined in "CLaSH.Prelude".

  This module uses the explicitly clocked 'CSignal's synchronous signals, as
  opposed to the implicitly clocked 'Signal's used in "CLaSH.Prelude". Take a
  look at "CLaSH.Signal.Explicit" to see how you can make multi-clock designs
  using explicitly clocked signals.
-}
module CLaSH.Prelude.Explicit
  ( -- * Creating synchronous sequential circuits
    sync
  , cregisterP
    -- * Utility functions
  , cwindow
  , cwindowD
    -- * Testbench functions
  , csassert
  , cstimuliGenerator
  , coutputVerifier
    -- * Exported modules
    -- ** Explicitly clocked synchronous signals
  , module CLaSH.Signal.Explicit
  )
where

import Data.Default          (Default (..))
import Debug.Trace           (trace)
import Control.Applicative   (Applicative (..), (<$>),liftA3)
import GHC.TypeLits          (KnownNat, type (+))

import CLaSH.Signal.Explicit
import CLaSH.Sized.Unsigned  (Unsigned)
import CLaSH.Sized.Vector    (Vec (..), (!), (+>>), maxIndex, vcopyI)

{-# INLINABLE sync #-}
-- | Create a synchronous function from a combinational function describing
-- a mealy machine
--
-- > mac :: Int        -- Current state
-- >     -> (Int,Int)  -- Input
-- >     -> (Int,Int)  -- (Updated state, output)
-- > mac s (x,y) = (s',s)
-- >   where
-- >     s' = x * y + s
-- >
-- > clk100 = Clock d100
-- >
-- > topEntity :: (CSignal 100 Int, CSignal 100 Int) -> CSignal 100 Int
-- > topEntity = sync clk100 mac 0
--
-- >>> csimulateP clk100 clk100 topEntity [(1,1),(2,2),(3,3),(4,4),...
-- [0,1,5,14,30,...
--
-- Synchronous sequential functions can be composed just like their combinational counterpart:
--
-- > dualMac :: (CSignal 100 Int, CSignal 100 Int)
-- >         -> (CSignal 100 Int, CSignal 100 Int)
-- >         -> CSignal 100 Int
-- > dualMac (a,b) (x,y) = s1 + s2
-- >   where
-- >     s1 = sync clk100 mac 0 (a,b)
-- >     s2 = sync clk100 mac 0 (x,y)
sync :: (CPack i, CPack o)
     => Clock clk                          -- ^ 'Clock' to synchronize to
     -> (s -> i -> (s,o))                  -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
     -> s                                  -- ^ Initial state
     -> (CSignalP clk i -> CSignalP clk o) -- ^ Synchronous sequential function with input and output matching that of the mealy machine
sync clk f iS = \i -> let (s',o) = cunpack clk $ f <$> s <*> cpack clk i
                          s      = cregister clk iS s'
                      in cunpack clk o

{-# INLINABLE cregisterP #-}
-- | Create a 'register' function for product-type like signals (e.g. '(Signal a, Signal b)')
--
-- > clk100 = Clock d100
-- >
-- > rP :: (CSignal 100 Int, CSignal 100 Int) -> (CSignal 100 Int, CSignal 100 Int)
-- > rP = cregisterP d100 (8,8)
--
-- >>> csimulateP clk100 clk100 rP [(1,1),(2,2),(3,3),...
-- [(8,8),(1,1),(2,2),(3,3),...
cregisterP :: CPack a => Clock clk -> a -> CSignalP clk a -> CSignalP clk a
cregisterP clk i = cunpack clk Prelude.. cregister clk i Prelude.. cpack clk

{-# INLINABLE cwindow #-}
-- | Give a window over a 'CSignal'
--
-- > window4 :: Signal Int -> Vec 4 (Signal Int)
-- > window4 = window
--
-- >>> csimulateP window4 [1,2,3,4,5,...
-- [<1,0,0,0>, <2,1,0,0>, <3,2,1,0>, <4,3,2,1>, <5,4,3,2>,...
cwindow :: (KnownNat (n + 1), Default a)
        => Clock clk                         -- ^ Clock to which the incoming signal is synchronized
        -> CSignal clk a                     -- ^ Signal to create a window over
        -> Vec ((n + 1) + 1) (CSignal clk a) -- ^ Window of at least size 2
cwindow clk x = x :> prev
  where
    prev = cregisterP clk (vcopyI def) next
    next = x +>> prev

{-# INLINABLE cwindowD #-}
-- | Give a delayed window over a 'CSignal'
--
-- > windowD3 :: Signal Int -> Vec 3 (Signal Int)
-- > windowD3 = windowD
--
-- >>> csimulateP windowD3 [1,2,3,4,...
-- [<0,0,0>, <1,0,0>, <2,1,0>, <3,2,1>, <4,3,2>,...
cwindowD :: (KnownNat (n + 1), Default a)
        => Clock clk                    -- ^ Clock to which the incoming signal is synchronized
        -> CSignal clk a                -- ^ Signal to create a window over
        -> Vec (n + 1) (CSignal clk a)  -- ^ Window of at least size 1
cwindowD clk x = prev
  where
    prev = cregisterP clk (vcopyI def) next
    next = x +>> prev

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
                            else trace ("\nexpected value: " ++ show b' ++ ", not equal to actual value: " ++ show a') c')

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
                  => Vec l a        -- ^ Samples to generate
                  -> Clock clk      -- ^ Clock to synchronize the output signal to
                  -> CSignal clk a  -- ^ Signal of given samples
cstimuliGenerator samples clk =
    let (r,o) = cunpack clk (genT <$> cregister clk (fromInteger (maxIndex samples)) r)
    in  o
  where
    genT :: Unsigned l -> (Unsigned l,a)
    genT s = (s',samples ! s)
      where
        s' = if s > 0 then s - 1
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
                => Vec l a           -- ^ Samples to compare with
                -> Clock clk         -- ^ Clock the input signal is synchronized to
                -> CSignal clk a     -- ^ Signal to verify
                -> CSignal clk Bool  -- ^ Indicator that all samples are verified
coutputVerifier samples clk i =
    let (s,o) = cunpack clk (genT <$> cregister clk (fromInteger (maxIndex samples)) s)
        (e,f) = cunpack clk o
    in  csassert i e (cregister clk False f)
  where
    genT :: Unsigned l -> (Unsigned l,(a,Bool))
    genT s = (s',(samples ! s,finished))
      where
        s' = if s >= 1 then s - 1
                       else s

        finished = s == 0
