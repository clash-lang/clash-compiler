{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -O0 -fno-omit-interface-pragmas #-}

module CLaSH.Prelude
  ( module Exported
  , (<^>)
  , registerP
  , Comp (..)
  , registerC
  , simulateC
  , (^^^)
  , blockRam
  , blockRamPow2
  , window
  , windowD
  )
where

import Control.Arrow               as Exported
import Control.Applicative         as Exported
import Control.Category            as Category
import Data.Bits                   as Exported
import Data.Default                as Exported
import CLaSH.Class.BitVector       as Exported
import CLaSH.Promoted.Bool         as Exported
import CLaSH.Promoted.Nat          as Exported
import CLaSH.Promoted.Nat.TH       as Exported
import CLaSH.Promoted.Nat.Literals as Exported
import CLaSH.Promoted.Ord          as Exported
import CLaSH.Sized.Signed          as Exported
import CLaSH.Sized.Unsigned        as Exported
import CLaSH.Sized.Vector          as Exported
import CLaSH.Bit                   as Exported
import CLaSH.Signal                as Exported
import GHC.TypeLits                as Exported

{-# INLINABLE window #-}
-- | Give a window over a 'Signal'
--
-- > window4 :: Signal Int -> Vec 4 (Signal Int)
-- > window4 = window
-- >
-- > simulateP window4 [1,2,3,4,5,... = [<1,0,0,0>,<2,1,0,0>,<3,2,1,0>,<4,3,2,1>,<5,4,3,2>,...
window :: (KnownNat (n + 1), Default a)
       => Signal a
       -> Vec ((n + 1) + 1) (Signal a)
window x = x :> prev
  where
    prev = registerP (vcopyI def) next
    next = x +>> prev

{-# INLINABLE windowD #-}
-- | Give a delayed window over a 'Signal'
--
-- > windowD3 :: Signal Int -> Vec 3 (Signal Int)
-- > windowD3 = windowD
-- >
-- > simulateP windowD3 [1,2,3,4,... = [<0,0,0>,<1,0,0>,<2,1,0>,<3,2,1>,<4,3,2>,...
windowD :: (KnownNat (n + 1), Default a)
        => Signal a
        -> Vec (n + 1) (Signal a)
windowD x = prev
  where
    prev = registerP (vcopyI def) next
    next = x +>> prev

{-# INLINABLE (<^>) #-}
-- | Create a synchronous function from a combinational function describing
-- a mealy machine
--
-- > mac s (x,y) = (s',s)
-- >   where
-- >     s' = x * y + s
-- >
-- > topEntity :: (Signal Int, Signal Int) -> Signal Int
-- > topEntity = mac <^> 0
-- >
-- > simulateP topEntity [(1,1),(2,2),(3,3),(4,4),... = [0,1,5,14,30,...
(<^>) :: (Pack i, Pack o)
      => (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form
      -> s -- ^ Initial state
      -> (SignalP i -> SignalP o) -- ^ Synchronous function with input and output matching that of the mealy machine
f <^> iS = \i -> let (s',o) = unpack $ f <$> s <*> (pack i)
                     s      = register iS s'
                 in unpack o

{-# INLINABLE registerP #-}
-- | Create a 'register' function for product-type like signals (e.g. '(Signal a, Signal b)')
--
-- > rP :: (Signal Int,Signal Int) -> (Signal Int, Signal Int)
-- > rP = registerP (8,8)
-- >
-- > simulateP rP [(1,1),(2,2),(3,3),... = [(8,8),(1,1),(2,2),(3,3),...
registerP :: Pack a => a -> SignalP a -> SignalP a
registerP i = unpack Prelude.. register i Prelude.. pack

{-# NOINLINE blockRam #-}
-- | Create a blockRAM with space for @n@ elements
--
-- > bram40 :: Signal (Unsigned 6) -> Signal (Unsigned 6) -> Signal Bool -> Signal a -> Signal a
-- > bram40 = blockRam d50
blockRam :: forall n m a . (KnownNat n, KnownNat m, Pack a)
         => SNat n -- ^ Size @n@ of the blockram
         -> Signal (Unsigned m) -- ^ Write address @w@
         -> Signal (Unsigned m) -- ^ Read address @r@
         -> Signal Bool -- ^ Write enable
         -> Signal a -- ^ Value to write (at address @w@)
         -> Signal a -- ^ Value of the 'blockRAM' at address @r@ from the previous clock cycle
blockRam n wr rd en din = pack $ (bram' <^> binit) (wr,rd,en,din)
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

{-# INLINABLE blockRamPow2 #-}
-- | Create a blockRAM with space for 2^@n@ elements
--
-- > bram32 :: Signal (Unsigned 5) -> Signal (Unsigned 5) -> Signal Bool -> Signal a -> Signal a
-- > bram32 = blockRamPow2 d32
blockRamPow2 :: (KnownNat n, KnownNat (2^n), Pack a)
             => (SNat ((2^n) :: Nat))  -- ^ Size 2^@n@ of the blockram
             -> Signal (Unsigned n) -- ^ Write address @w@
             -> Signal (Unsigned n) -- ^ Read address @r@
             -> Signal Bool -- ^ Write enable
             -> Signal a -- ^ Value to write (at address @w@)
             -> Signal a -- ^ Value of the 'blockRAM' at address @r@ from the previous clock cycle
blockRamPow2 = blockRam

-- | 'Arrow' interface to synchronous functions
newtype Comp a b = C { asFunction :: Signal a -> Signal b }

instance Category Comp where
  id            = C Prelude.id
  (C f) . (C g) = C (f Prelude.. g)

infixr 8 ><
(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f >< g) (x,y) = (f x,g y)

instance Arrow Comp where
  arr         = C Prelude.. fmap
  first (C f) = C $ pack Prelude.. (f >< Prelude.id) Prelude.. unpack

instance ArrowLoop Comp where
  loop (C f) = C $ simpleLoop (unpack Prelude.. f Prelude.. pack)
    where
      simpleLoop g b = let ~(c,d) = g (b,d)
                       in c

-- | Create a 'register' 'Comp'onent
--
-- > rC :: Comp (Int,Int) (Int,Int)
-- > rC = registerC (8,8)
-- >
-- > simulateC rP [(1,1),(2,2),(3,3),... = [(8,8),(1,1),(2,2),(3,3),...
registerC :: a -> Comp a a
registerC = C Prelude.. register

-- | Simulate a 'Comp'onent given a list of samples
--
-- > simulateC (registerC 8) [1, 2, 3, ... = [8, 1, 2, 3, ...
simulateC :: Comp a b -> [a] -> [b]
simulateC f = simulate (asFunction f)

{-# INLINABLE (^^^) #-}
-- | Create a synchronous 'Comp'onent from a combinational function describing
-- a mealy machine
--
-- > mac s (x,y) = (s',s)
-- >   where
-- >     s' = x * y + s
-- >
-- > topEntity :: Comp (Int,Int) Int
-- > topEntity = mac ^^^ 0
-- >
-- > simulateC topEntity [(1,1),(2,2),(3,3),(4,4),... = [0,1,5,14,30,...
(^^^) :: (s -> i -> (s,o)) -> s -> Comp i o
f ^^^ sI = C $ \i -> let (s',o) = unpack $ f <$> s <*> i
                         s      = register sI s'
                     in  o
