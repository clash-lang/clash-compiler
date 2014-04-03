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
    -- * Exported modules
    -- ** Explicitly clocked synchronous signals
  , module CLaSH.Signal.Explicit
  )
where

import Data.Default          (Default (..))
import Control.Applicative   (Applicative (..), (<$>))
import Control.Arrow         (Arrow (..), ArrowLoop (..))
import Control.Category      as Category
import GHC.TypeLits          (KnownNat,type (^), type (+))

import CLaSH.Promoted.Nat    (SNat, snat)
import CLaSH.Signal.Explicit
import CLaSH.Sized.Unsigned  (Unsigned)
import CLaSH.Sized.Vector    (Vec (..), (!), (+>>), vcopy, vcopyI, vreplace)

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
-- > clk100 = Clock d100
-- >
-- > bram40 :: CSignal 100 (Unsigned 6) -> CSignal 100 (Unsigned 6)
-- >        -> CSignal 100 Bool -> CSignal 100 a -> 100 CSignal a
-- > bram40 = cblockRam clk100 d50
cblockRam :: forall n m a clk . (KnownNat n, KnownNat m, CPack a)
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
    binit = (vcopy n (error "uninitialized ram"),error "uninitialized ram")

    bram' :: (Vec n a,a) -> (Unsigned m, Unsigned m, Bool, a)
          -> (((Vec n a),a),a)
    bram' (ram,o) (w,r,e,d) = ((ram',o'),o)
      where
        ram' | e         = vreplace ram w d
             | otherwise = ram
        o'               = ram ! r

-- | Create a blockRAM with space for @n@ elements
--
-- > clk100 = Clock 100
-- >
-- > bramC40 :: CComp 100 (Unsigned 6, Unsigned 6, Bool, a) a
-- > bramC40 = blockRamCC clk100 d50
blockRamCC :: (KnownNat n, KnownNat m, CPack a)
           => Clock clk -- ^ 'Clock' to synchronize to
           -> SNat n    -- ^ Size @n@ of the blockram
           -> CComp clk (Unsigned m, Unsigned m, Bool, a) a
blockRamCC clk n = CC ((\(wr,rd,en,din) -> cblockRam clk n wr rd en din) Prelude.. cunpack clk)

{-# INLINABLE cblockRamPow2 #-}
-- | Create a blockRAM with space for 2^@n@ elements
--
-- > bram32 :: Signal (Unsigned 5) -> Signal (Unsigned 5) -> Signal Bool -> Signal a -> Signal a
-- > bram32 = cblockRamPow2 d32
cblockRamPow2 :: (KnownNat n, KnownNat (2^n), CPack a)
              => Clock clk                -- ^ 'Clock' to synchronize to
              -> SNat (2^n)               -- ^ Size @2^n@ of the blockram
              -> CSignal clk (Unsigned n) -- ^ Write address @w@
              -> CSignal clk (Unsigned n) -- ^ Read address @r@
              -> CSignal clk Bool         -- ^ Write enable
              -> CSignal clk a            -- ^ Value to write (at address @w@)
              -> CSignal clk a            -- ^ Value of the 'blockRAM' at address @r@ from the previous clock cycle
cblockRamPow2 = cblockRam

-- | Create a blockRAM with space for 2^@n@ elements
--
-- > clk100 = Clock d100
-- >
-- > bramC32 :: CComp 100 (Unsigned 5, Unsigned 5, Bool, a) a
-- > bramC32 = blockRamPow2CC clk100 d32
blockRamPow2CC :: (KnownNat n, KnownNat (2^n), CPack a)
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
        => Clock clk
        -> CSignal clk a
        -> Vec ((n + 1) + 1) (CSignal clk a)
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
        => Clock clk
        -> CSignal clk a
        -> Vec (n + 1) (CSignal clk a)
cwindowD clk x = prev
  where
    prev = cregisterP clk (vcopyI def) next
    next = x +>> prev
