{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitNamespaces  #-}
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
    -- * 'Arrow' interface for synchronous sequential circuits
  , CComp (..)
  , syncA
  , cregisterC
  , csimulateC
    -- * BlockRAM primitives
  , cblockRam
  , cblockRamPow2
  , blockRamCC
  , blockRamPow2CC
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
import Control.Arrow         (Arrow (..), ArrowLoop (..))
import Control.Category      as Category
import GHC.TypeLits          (KnownNat,type (^), type (+))

import CLaSH.Promoted.Nat    (SNat, snat)
import CLaSH.Signal.Explicit
import CLaSH.Sized.Unsigned  (Unsigned)
import CLaSH.Sized.Vector    (Vec (..), (!), (+>>), maxIndex, vcopy, vcopyI, vreplace)

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

{-# DEPRECATED CComp "Use 'Applicative' interface and 'sync' instead" #-}
-- | 'CComp'onent: an 'Arrow' interface to explicitly clocked synchronous
-- sequential functions
newtype CComp t a b = CC { asCFunction :: CSignal t a -> CSignal t b }

instance Category (CComp t) where
  id              = CC Prelude.id
  (CC f) . (CC g) = CC (f Prelude.. g)

instance KnownNat t => Arrow (CComp t) where
  arr          = CC Prelude.. fmap
  first (CC f) = let clk = Clock snat
                 in  CC $ cpack clk Prelude.. (f >< Prelude.id) Prelude.. cunpack clk
    where
      (g >< h) (x,y) = (g x,h y)

instance KnownNat t => ArrowLoop (CComp t) where
  loop (CC f) = let clk = Clock snat
                in  CC $ simpleLoop (cunpack clk Prelude.. f Prelude.. cpack clk)
    where
      simpleLoop g b = let ~(c,d) = g (b,d)
                       in c

{-# DEPRECATED syncA "Use 'Applicative' interface and 'sync' instead" #-}
{-# INLINABLE syncA #-}
-- | Create a synchronous 'CComp'onent from a combinational function describing
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
-- > topEntity :: CComp 100 (Int,Int) Int
-- > topEntity = syncA clk100 mac 0
--
-- >>> simulateC topEntity [(1,1),(2,2),(3,3),(4,4),...
-- [0,1,5,14,30,...
--
-- Synchronous sequential must be composed using the 'Arrow' syntax
--
-- > dualMac :: CComp 100 (Int,Int,Int,Int) Int
-- > dualMac = proc (a,b,x,y) -> do
-- >   rec s1 <- syncA clk100 mac 0 -< (a,b)
-- >       s2 <- syncA clk100 mac 0 -< (x,y)
-- >   returnA -< (s1 + s2)
syncA :: Clock clk         -- ^ 'Clock' to synchronize to
      -> (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
      -> s                 -- ^ Initial state
      -> CComp clk i o     -- ^ Synchronous sequential 'Comp'onent with input and output matching that of the mealy machine
syncA clk f sI = CC $ \i -> let (s',o) = cunpack clk $ f <$> s <*> i
                                s      = cregister clk sI s'
                            in  o

{-# DEPRECATED cregisterC "'CComp' is deprecated, use 'cregister' instead" #-}
-- | Create a 'cregister' 'CComp'onent
--
-- > clk100 = Clock d100
-- >
-- > rC :: CComp 100 (Int,Int) (Int,Int)
-- > rC = cregisterC clk100 (8,8)
--
-- >>> simulateC rP [(1,1),(2,2),(3,3),...
-- [(8,8),(1,1),(2,2),(3,3),...
cregisterC :: Clock clk -> a -> CComp clk a a
cregisterC clk = CC Prelude.. cregister clk

{-# DEPRECATED csimulateC "'CComp' is deprecated, use 'csimulate' instead" #-}
-- | Simulate a 'Comp'onent given a list of samples
--
-- > clk100 = Clock d100
-- >>> csimulateC (cregisterC clk100 8) [1, 2, 3, ...
-- [8, 1, 2, 3, ...
csimulateC :: CComp clk a b -> [a] -> [b]
csimulateC f = csimulate (asCFunction f)

{-# NOINLINE cblockRam #-}
-- | Create a blockRAM with space for @n@ elements
--
-- NB: Read value is delayed by 1 cycle
--
-- > clk100 = Clock d100
-- >
-- > bram40 :: CSignal 100 (Unsigned 6) -> CSignal 100 (Unsigned 6)
-- >        -> CSignal 100 Bool -> CSignal 100 a -> 100 CSignal a
-- > bram40 = cblockRam clk100 d50
cblockRam :: forall n m a clk . (KnownNat n, KnownNat m, CPack a, Default a)
          => Clock clk                -- ^ 'Clock' to synchronize to
          -> SNat n                   -- ^ Size @n@ of the blockram
          -> CSignal clk (Unsigned m) -- ^ Write address @w@
          -> CSignal clk (Unsigned m) -- ^ Read address @r@
          -> CSignal clk Bool         -- ^ Write enable
          -> CSignal clk a            -- ^ Value to write (at address @w@)
          -> CSignal clk a            -- ^ Value of the 'blockRAM' at address @r@ from the previous clock cycle
cblockRam clk n wr rd en din = cpack clk $ (sync clk bram' binit) (wr,rd,en,din)
  where
    binit :: (Vec n a,a)
    binit = (vcopy n def,def)

    bram' :: (Vec n a,a) -> (Unsigned m, Unsigned m, Bool, a)
          -> (((Vec n a),a),a)
    bram' (ram,o) (w,r,e,d) = ((ram',o'),o)
      where
        ram' | e         = vreplace ram w d
             | otherwise = ram
        o'               = ram ! r

{-# DEPRECATED blockRamCC "'CComp' is deprecated, use 'cblockRam' instead" #-}
-- | Create a blockRAM with space for @n@ elements
--
-- NB: Read value is delayed by 1 cycle
--
-- > clk100 = Clock 100
-- >
-- > bramC40 :: CComp 100 (Unsigned 6, Unsigned 6, Bool, a) a
-- > bramC40 = blockRamCC clk100 d50
blockRamCC :: (KnownNat n, KnownNat m, CPack a, Default a)
           => Clock clk -- ^ 'Clock' to synchronize to
           -> SNat n    -- ^ Size @n@ of the blockram
           -> CComp clk (Unsigned m, Unsigned m, Bool, a) a
blockRamCC clk n = CC ((\(wr,rd,en,din) -> cblockRam clk n wr rd en din) Prelude.. cunpack clk)

{-# INLINABLE cblockRamPow2 #-}
-- | Create a blockRAM with space for 2^@n@ elements
--
-- NB: Read value is delayed by 1 cycle
--
-- > bram32 :: Signal (Unsigned 5) -> Signal (Unsigned 5) -> Signal Bool -> Signal a -> Signal a
-- > bram32 = cblockRamPow2 d32
cblockRamPow2 :: (KnownNat n, KnownNat (2^n), CPack a, Default a)
              => Clock clk                -- ^ 'Clock' to synchronize to
              -> SNat (2^n)               -- ^ Size @2^n@ of the blockram
              -> CSignal clk (Unsigned n) -- ^ Write address @w@
              -> CSignal clk (Unsigned n) -- ^ Read address @r@
              -> CSignal clk Bool         -- ^ Write enable
              -> CSignal clk a            -- ^ Value to write (at address @w@)
              -> CSignal clk a            -- ^ Value of the 'blockRAM' at address @r@ from the previous clock cycle
cblockRamPow2 = cblockRam

{-# DEPRECATED blockRamPow2CC "'CComp' is deprecated, use 'cblockRamPow2' instead" #-}
-- | Create a blockRAM with space for 2^@n@ elements
--
-- NB: Read value is delayed by 1 cycle
--
-- > clk100 = Clock d100
-- >
-- > bramC32 :: CComp 100 (Unsigned 5, Unsigned 5, Bool, a) a
-- > bramC32 = blockRamPow2CC clk100 d32
blockRamPow2CC :: (KnownNat n, KnownNat (2^n), CPack a, Default a)
               => Clock clk  -- ^ 'Clock' to synchronize to
               -> SNat (2^n) -- ^ Size @2^n@ of the blockram
               -> CComp clk (Unsigned n, Unsigned n, Bool, a) a
blockRamPow2CC clk n = CC ((\(wr,rd,en,din) -> cblockRamPow2 clk n wr rd en din) Prelude.. cunpack clk)

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
