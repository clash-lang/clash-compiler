{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Trustworthy #-}

{-|
Copyright  :  (C) 2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

RAM primitives with a combinational read port
-}
module CLaSH.Prelude.RAM
  ( -- * RAM synchronised to the system clock
    asyncRam
  , asyncRamPow2
    -- * RAM synchronised to an arbitrary clock
  , asyncRam'
  , asyncRamPow2'
    -- * Internal
  , asyncRam#
  )
where

import Control.Monad          (when)
import Control.Monad.ST.Lazy  (ST,runST)
import Data.Array.MArray      (newArray_,readArray,writeArray)
import Data.Array.ST          (STArray)
import GHC.TypeLits           (KnownNat, KnownSymbol, type (^))

import CLaSH.Promoted.Nat     (SNat,snat,snatToInteger)
import CLaSH.Signal           (Signal)
import CLaSH.Signal.Bundle    (bundle')
import CLaSH.Signal.Explicit  (Clock (..), Signal', SClock, sclock, systemClock,
                               unsafeSynchronizer)
import CLaSH.Sized.Unsigned   (Unsigned)

{-# INLINE asyncRam #-}
-- | Create a RAM with space for @n@ elements.
--
-- * __NB__: Initial content of the RAM is 'undefined'
asyncRam :: (KnownNat n, Enum addr)
         => SNat n      -- ^ Size @n@ of the RAM
         -> Signal addr -- ^ Write address @w@
         -> Signal addr -- ^ Read address @r@
         -> Signal Bool -- ^ Write enable
         -> Signal a    -- ^ Value to write (at address @w@)
         -> Signal a
         -- ^ Value of the @AM@ at address @r@
asyncRam = asyncRam' systemClock

{-# INLINE asyncRamPow2 #-}
-- | Create a RAM with space for 2^@n@ elements
--
-- * __NB__: Initial content of the RAM is 'undefined'
asyncRamPow2 :: forall n a . (KnownNat (2^n), KnownNat n)
             => Signal (Unsigned n) -- ^ Write address @w@
             -> Signal (Unsigned n) -- ^ Read address @r@
             -> Signal Bool         -- ^ Write enable
             -> Signal a            -- ^ Value to write (at address @w@)
             -> Signal a
             -- ^ Value of the @RAM@ at address @r@
asyncRamPow2 = asyncRam' systemClock (snat :: SNat (2^n))

{-# INLINE asyncRamPow2' #-}
-- | Create a RAM with space for 2^@n@ elements
--
-- * __NB__: Initial content of the RAM is 'undefined'
asyncRamPow2' :: forall wclk rclk name period n a .
                 (KnownNat n, KnownNat (2^n), KnownSymbol name, KnownNat period
                 ,'Clk name period ~ rclk)
              => SClock wclk               -- ^ 'Clock' to synchronize to
              -> Signal' wclk (Unsigned n) -- ^ Write address @w@
              -> Signal' rclk (Unsigned n) -- ^ Read address @r@
              -> Signal' wclk Bool         -- ^ Write enable
              -> Signal' wclk a            -- ^ Value to write (at address @w@)
              -> Signal' rclk a
              -- ^ Value of the @RAM@ at address @r@
asyncRamPow2' wclk = asyncRam' wclk (snat :: SNat (2^n))

{-# INLINE asyncRam' #-}
-- | Create a RAM with space for @n@ elements
--
-- * __NB__: Initial content of the RAM is 'undefined'
asyncRam' :: (KnownNat n, KnownSymbol name, KnownNat period
             ,'Clk name period ~ rclk, Enum addr)
          => SClock wclk       -- ^ 'Clock' to synchronize to
          -> SNat n            -- ^ Size @n@ of the RAM
          -> Signal' wclk addr -- ^ Write address @w@
          -> Signal' rclk addr -- ^ Read address @r@
          -> Signal' wclk Bool -- ^ Write enable
          -> Signal' wclk a    -- ^ Value to write (at address @w@)
          -> Signal' rclk a    -- ^ Value of the @RAM@ at address @r@
asyncRam' wclk sz wr rd en din = asyncRam# wclk sz (fromEnum <$> wr)
                                           (fromEnum <$> rd) en din

{-# NOINLINE asyncRam# #-}
-- | RAM primitive
asyncRam# :: (KnownSymbol name, KnownNat period ,'Clk name period ~ rclk)
          => SClock wclk       -- ^ 'Clock' to synchronize to
          -> SNat n            -- ^ Size @n@ of the RAM
          -> Signal' wclk Int  -- ^ Write address @w@
          -> Signal' rclk Int  -- ^ Read address @r@
          -> Signal' wclk Bool -- ^ Write enable
          -> Signal' wclk a    -- ^ Value to write (at address @w@)
          -> Signal' rclk a    -- ^ Value of the @RAM@ at address @r@
asyncRam# wclk sz wr rd en din = unsafeSynchronizer wclk sclock dout
  where
    szI  = fromInteger $ snatToInteger sz
    rd'  = unsafeSynchronizer sclock wclk rd
    dout = runST $ do
      arr <- newArray_ (0,szI-1)
      traverse (ramT arr) (bundle' wclk (wr,rd',en,din))

    ramT :: STArray s Int e -> (Int,Int,Bool,e) -> ST s e
    ramT ram (w,r,e,d) = do
      d' <- readArray ram r
      when e (writeArray ram w d)
      return d'
