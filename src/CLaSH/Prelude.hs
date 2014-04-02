{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -O0 -fno-omit-interface-pragmas #-}

module CLaSH.Prelude
  ( -- * Creating synchronous sequential circuits
    (<^>)
  , registerP
    -- * 'Arrow' interface for synchronous sequential circuits
  , Comp (..)
  , (^^^)
  , registerC
  , simulateC
    -- * BlockRAM primitives
  , blockRam
  , blockRamPow2
  , blockRamC
  , blockRamPow2C
    -- * Utility functions
  , window
  , windowD
    -- * Exported modules
    -- ** Synchronous signals
  , module CLaSH.Signal
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
    -- ** Type classes
    -- *** CLaSH
  , module CLaSH.Class.BitVector
  , module CLaSH.Class.Num
    -- *** Other
  , module Control.Arrow
  , module Control.Applicative
  , module Data.Bits
  , module Data.Default
  )
where

import Control.Arrow
import Control.Applicative
import Control.Category            as Category
import Data.Bits
import Data.Default
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
import CLaSH.Signal
import GHC.TypeLits

{-# INLINABLE window #-}
-- | Give a window over a 'Signal'
--
-- > window4 :: Signal Int -> Vec 4 (Signal Int)
-- > window4 = window
--
-- >>> simulateP window4 [1,2,3,4,5,...
-- [<1,0,0,0>, <2,1,0,0>, <3,2,1,0>, <4,3,2,1>, <5,4,3,2>,...
window :: (KnownNat (n + 1), Default a, ?clk :: Clock t)
       => Signal t a
       -> Vec ((n + 1) + 1) (Signal t a)
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
windowD :: (KnownNat (n + 1), Default a, ?clk :: Clock t)
        => Signal t a
        -> Vec (n + 1) (Signal t a)
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
(<^>) :: (Pack i, Pack o, ?clk :: Clock t)
      => (s -> i -> (s,o))            -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
      -> s                            -- ^ Initial state
      -> (SignalP t i -> SignalP t o) -- ^ Synchronous sequential function with input and output matching that of the mealy machine
f <^> iS = \i -> let (s',o) = unpack ?clk $ f <$> s <*> pack ?clk i
                     s      = register iS s'
                 in unpack ?clk o

{-# INLINABLE registerP #-}
-- | Create a 'register' function for product-type like signals (e.g. '(Signal a, Signal b)')
--
-- > rP :: (Signal Int,Signal Int) -> (Signal Int, Signal Int)
-- > rP = registerP (8,8)
--
-- >>> simulateP rP [(1,1),(2,2),(3,3),...
-- [(8,8),(1,1),(2,2),(3,3),...
registerP :: (Pack a, ?clk :: Clock t) => a -> SignalP t a -> SignalP t a
registerP i = unpack ?clk Prelude.. register i Prelude.. pack ?clk

{-# NOINLINE blockRam #-}
-- | Create a blockRAM with space for @n@ elements
--
-- > bram40 :: Signal (Unsigned 6) -> Signal (Unsigned 6) -> Signal Bool -> Signal a -> Signal a
-- > bram40 = blockRam d50
blockRam :: forall n m a t . (KnownNat n, KnownNat m, Pack a, ?clk :: Clock t)
         => SNat n                -- ^ Size @n@ of the blockram
         -> Signal t (Unsigned m) -- ^ Write address @w@
         -> Signal t (Unsigned m) -- ^ Read address @r@
         -> Signal t Bool         -- ^ Write enable
         -> Signal t a            -- ^ Value to write (at address @w@)
         -> Signal t a            -- ^ Value of the 'blockRAM' at address @r@ from the previous clock cycle
blockRam n wr rd en din = pack ?clk $ (bram' <^> binit) (wr,rd,en,din)
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
-- > bramC40 :: Comp (Unsigned 6, Unsigned 6, Bool, a) a
-- > bramC40 = blockRamC d50
blockRamC :: (KnownNat n, KnownNat m, Pack a, ?clk :: Clock t)
          => SNat n -- ^ Size @n@ of the blockram
          -> Comp t (Unsigned m, Unsigned m, Bool, a) a
blockRamC n = C ((\(wr,rd,en,din) -> blockRam n wr rd en din) Prelude.. unpack ?clk)

{-# INLINABLE blockRamPow2 #-}
-- | Create a blockRAM with space for 2^@n@ elements
--
-- > bram32 :: Signal (Unsigned 5) -> Signal (Unsigned 5) -> Signal Bool -> Signal a -> Signal a
-- > bram32 = blockRamPow2 d32
blockRamPow2 :: (KnownNat n, KnownNat (2^n), Pack a, ?clk :: Clock t)
             => SNat (2^n)          -- ^ Size @2^n@ of the blockram
             -> Signal t (Unsigned n) -- ^ Write address @w@
             -> Signal t (Unsigned n) -- ^ Read address @r@
             -> Signal t Bool         -- ^ Write enable
             -> Signal t a            -- ^ Value to write (at address @w@)
             -> Signal t a            -- ^ Value of the 'blockRAM' at address @r@ from the previous clock cycle
blockRamPow2 = blockRam

-- | Create a blockRAM with space for 2^@n@ elements
--
-- > bramC32 :: Comp (Unsigned 5, Unsigned 5, Bool, a) a
-- > bramC32 = blockRamPow2C d32
blockRamPow2C :: (KnownNat n, KnownNat (2^n), Pack a, ?clk :: Clock t)
              => SNat (2^n) -- ^ Size @2^n@ of the blockram
              -> Comp t (Unsigned n, Unsigned n, Bool, a) a
blockRamPow2C n = C ((\(wr,rd,en,din) -> blockRamPow2 n wr rd en din) Prelude.. unpack ?clk)

-- | 'Comp'onent: an 'Arrow' interface to synchronous sequential functions
newtype Comp t a b = C { asFunction :: Signal t a -> Signal t b }

instance Category (Comp t) where
  id            = C Prelude.id
  (C f) . (C g) = C (f Prelude.. g)

infixr 8 ><
(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f >< g) (x,y) = (f x,g y)

instance (?clk :: Clock t) => Arrow (Comp t) where
  arr         = C Prelude.. fmap
  first (C f) = C $ pack ?clk Prelude.. (f >< Prelude.id) Prelude.. unpack ?clk

instance (?clk :: Clock t) => ArrowLoop (Comp t) where
  loop (C f) = C $ simpleLoop (unpack ?clk Prelude.. f Prelude.. pack ?clk)
    where
      simpleLoop g b = let ~(c,d) = g (b,d)
                       in c

-- | Create a 'register' 'Comp'onent
--
-- > rC :: Comp (Int,Int) (Int,Int)
-- > rC = registerC (8,8)
--
-- >>> simulateC rP [(1,1),(2,2),(3,3),...
-- [(8,8),(1,1),(2,2),(3,3),...
registerC :: (?clk :: Clock t) => a -> Comp t a a
registerC = C Prelude.. register

-- | Simulate a 'Comp'onent given a list of samples
--
-- >>> simulateC (registerC 8) [1, 2, 3, ...
-- [8, 1, 2, 3, ...
simulateC :: (?clk :: Clock t) => Comp t a b -> [a] -> [b]
simulateC f = simulate (asFunction f)

{-# INLINABLE (^^^) #-}
-- | Create a synchronous 'Comp'onent from a combinational function describing
-- a mealy machine
--
-- > mac :: Int        -- Current state
-- >     -> (Int,Int)  -- Input
-- >     -> (Int,Int)  -- (Updated state, output)
-- > mac s (x,y) = (s',s)
-- >   where
-- >     s' = x * y + s
-- >
-- > topEntity :: Comp (Int,Int) Int
-- > topEntity = mac ^^^ 0
--
-- >>> simulateC topEntity [(1,1),(2,2),(3,3),(4,4),...
-- [0,1,5,14,30,...
--
-- Synchronous sequential must be composed using the 'Arrow' syntax
--
-- > dualMac :: Comp (Int,Int,Int,Int) Int
-- > dualMac = proc (a,b,x,y) -> do
-- >   rec s1 <- mac ^^^ 0 -< (a,b)
-- >       s2 <- mac ^^^ 0 -< (x,y)
-- >   returnA -< (s1 + s2)
(^^^) :: (?clk :: Clock t)
      => (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
      -> s                 -- ^ Initial state
      -> Comp t i o        -- ^ Synchronous sequential 'Comp'onent with input and output matching that of the mealy machine
f ^^^ sI = C $ \i -> let (s',o) = unpack ?clk $ f <$> s <*> i
                         s      = register sI s'
                     in  o
