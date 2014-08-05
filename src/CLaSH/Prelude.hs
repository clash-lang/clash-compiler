{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -O0 -fno-omit-interface-pragmas #-}

{- |
  CλaSH (pronounced ‘clash’) is a functional hardware description language that
  borrows both its syntax and semantics from the functional programming language
  Haskell. The merits of using a functional language to describe hardware comes
  from the fact that combinational circuits can be directly modeled as
  mathematical functions and that functional languages lend themselves very well
  at describing and (de-)composing mathematical functions.

  This package provides:

  * Prelude library containing datatypes and functions for circuit design

  To use the library:

  * Import "CLaSH.Prelude"
  * Additionally import "CLaSH.Prelude.Explicit" if you want to design
    explicitly clocked circuits in a multi-clock setting

  For now, "CLaSH.Prelude" is also the best starting point for exploring the
  library. A tutorial module will be added within due time.
-}
module CLaSH.Prelude
  ( -- * Creating synchronous sequential circuits
    (<^>)
  , registerP
    -- * Utility functions
  , window
  , windowD
    -- * Testbench functions
  , sassert
  , stimuliGenerator
  , outputVerifier
    -- * Exported modules
    -- ** Implicitly clocked synchronous signals
  , module CLaSH.Signal.Implicit
    -- ** Datatypes
  , module CLaSH.Bit
    -- *** Arbitrary-width numbers
  , module CLaSH.Sized.Signed
  , module CLaSH.Sized.Unsigned
    -- *** Fixed point numbers
  , module CLaSH.Sized.Fixed
    -- *** Fixed size vectors
  , module CLaSH.Sized.Vector
    -- ** Type-level natural numbers
  , module GHC.TypeLits
  , module CLaSH.Promoted.Nat
  , module CLaSH.Promoted.Nat.Literals
  , module CLaSH.Promoted.Nat.TH
    -- ** Type-level functions
  , module CLaSH.Promoted.Ord
    -- ** Template Haskell
  , Lift (..), deriveLift
    -- ** Type classes
    -- *** CLaSH
  , module CLaSH.Class.BitVector
  , module CLaSH.Class.Num
    -- *** Other
  , module Control.Applicative
  , module Data.Bits
  , module Data.Default
  )
where

import Control.Applicative
import Data.Bits
import Data.Default
import Debug.Trace                 (trace)
import CLaSH.Class.BitVector
import CLaSH.Class.Num
import CLaSH.Promoted.Nat
import CLaSH.Promoted.Nat.TH
import CLaSH.Promoted.Nat.Literals
import CLaSH.Promoted.Ord
import CLaSH.Sized.Fixed
import CLaSH.Sized.Signed
import CLaSH.Sized.Unsigned
import CLaSH.Sized.Vector
import CLaSH.Bit
import CLaSH.Signal.Implicit
import GHC.TypeLits
import Language.Haskell.TH.Lift     (Lift(..),deriveLift)

{-# INLINABLE window #-}
-- | Give a window over a 'Signal'
--
-- > window4 :: Signal Int -> Vec 4 (Signal Int)
-- > window4 = window
--
-- >>> simulateP window4 [1,2,3,4,5,...
-- [<1,0,0,0>, <2,1,0,0>, <3,2,1,0>, <4,3,2,1>, <5,4,3,2>,...
window :: (KnownNat (n + 1), Default a)
       => Signal a                     -- ^ Signal to create a window over
       -> Vec ((n + 1) + 1) (Signal a) -- ^ Window of at least size 2
window x = x :> prev
  where
    prev = registerP (vcopyI def) next
    next = x +>> prev

{-# INLINABLE windowD #-}
-- | Give a delayed window over a 'Signal'
--
-- > windowD3 :: Signal Int -> Vec 3 (Signal Int)
-- > windowD3 = windowD
--
-- >>> simulateP windowD3 [1,2,3,4,...
-- [<0,0,0>, <1,0,0>, <2,1,0>, <3,2,1>, <4,3,2>,...
windowD :: (KnownNat (n + 1), Default a)
        => Signal a               -- ^ Signal to create a window over
        -> Vec (n + 1) (Signal a) -- ^ Window of at least size 1
windowD x = prev
  where
    prev = registerP (vcopyI def) next
    next = x +>> prev

{-# INLINABLE (<^>) #-}
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
-- > topEntity :: (Signal Int, Signal Int) -> Signal Int
-- > topEntity = mac <^> 0
--
-- >>> simulateP topEntity [(1,1),(2,2),(3,3),(4,4),...
-- [0,1,5,14,30,...
--
-- Synchronous sequential functions can be composed just like their combinational counterpart:
--
-- > dualMac :: (Signal Int, Signal Int)
-- >         -> (Signal Int, Signal Int)
-- >         -> Signal Int
-- > dualMac (a,b) (x,y) = s1 + s2
-- >   where
-- >     s1 = (mac <^> 0) (a,b)
-- >     s2 = (mac <^> 0) (x,y)
(<^>) :: (Pack i, Pack o)
      => (s -> i -> (s,o))        -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
      -> s                        -- ^ Initial state
      -> (SignalP i -> SignalP o) -- ^ Synchronous sequential function with input and output matching that of the mealy machine
f <^> iS = \i -> let (s',o) = unpack $ f <$> s <*> pack i
                     s      = register iS s'
                 in unpack o

{-# INLINABLE registerP #-}
-- | Create a 'register' function for product-type like signals (e.g. '(Signal a, Signal b)')
--
-- > rP :: (Signal Int,Signal Int) -> (Signal Int, Signal Int)
-- > rP = registerP (8,8)
--
-- >>> simulateP rP [(1,1),(2,2),(3,3),...
-- [(8,8),(1,1),(2,2),(3,3),...
registerP :: Pack a => a -> SignalP a -> SignalP a
registerP i = unpack Prelude.. register i Prelude.. pack

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
sassert = liftA3
  (\a' b' c' -> if a' == b' then c'
                            else trace ("\nexpected value: " ++ show b' ++ ", not equal to actual value: " ++ show a') c')

{-# INLINABLE stimuliGenerator #-}
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
stimuliGenerator samples  =
    let (r,o) = unpack (genT <$> register (fromInteger (maxIndex samples)) r)
    in  o
  where
    genT :: Unsigned l -> (Unsigned l, a)
    genT s = (s',samples ! s)
      where
        s' = if s > 0 then s - 1
                      else s

{-# INLINABLE outputVerifier #-}
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
outputVerifier samples i =
    let (s,o) = unpack (genT <$> register (fromInteger (maxIndex samples)) s)
        (e,f) = unpack o
    in  sassert i e (register False f)
  where
    genT :: Unsigned l -> (Unsigned l, (a,Bool))
    genT s = (s',(samples ! s,finished))
      where
        s' = if s >= 1 then s - 1
                       else s

        finished = s == 0
