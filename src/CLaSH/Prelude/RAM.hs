{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

RAM primitives with a combinational read port.
-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Trustworthy #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

{-# OPTIONS_HADDOCK show-extensions #-}

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

import Control.Exception      (catch, evaluate, throw)
import Control.Monad          (when)
import Control.Monad.ST.Lazy  (ST,runST)
import Control.Monad.ST.Lazy.Unsafe (unsafeIOToST)
import Data.Array.MArray.Safe (newListArray,readArray,writeArray)
import Data.Array.ST.Safe     (STArray)
import GHC.TypeLits           (KnownNat)

import CLaSH.Promoted.Nat     (SNat (..), snatToNum, pow2SNat)
import CLaSH.Signal           (Signal)
import CLaSH.Signal.Bundle    (bundle)
import CLaSH.Signal.Explicit  (Signal', SClock, systemClock, unsafeSynchronizer)
import CLaSH.Sized.Unsigned   (Unsigned)
import CLaSH.XException       (XException, errorX)

{-# INLINE asyncRam #-}
-- | Create a RAM with space for @n@ elements.
--
-- * __NB__: Initial content of the RAM is 'undefined'
--
-- Additional helpful information:
--
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRam :: Enum addr
         => SNat n      -- ^ Size @n@ of the RAM
         -> Signal addr -- ^ Write address @w@
         -> Signal addr -- ^ Read address @r@
         -> Signal Bool -- ^ Write enable
         -> Signal a    -- ^ Value to write (at address @w@)
         -> Signal a    -- ^ Value of the @RAM@ at address @r@
asyncRam = asyncRam' systemClock systemClock

{-# INLINE asyncRamPow2 #-}
-- | Create a RAM with space for 2^@n@ elements
--
-- * __NB__: Initial content of the RAM is 'undefined'
--
-- Additional helpful information:
--
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRamPow2 :: KnownNat n
             => Signal (Unsigned n) -- ^ Write address @w@
             -> Signal (Unsigned n) -- ^ Read address @r@
             -> Signal Bool         -- ^ Write enable
             -> Signal a            -- ^ Value to write (at address @w@)
             -> Signal a            -- ^ Value of the @RAM@ at address @r@
asyncRamPow2 = asyncRamPow2' systemClock systemClock

{-# INLINE asyncRamPow2' #-}
-- | Create a RAM with space for 2^@n@ elements
--
-- * __NB__: Initial content of the RAM is 'undefined'
--
-- Additional helpful information:
--
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRamPow2' :: forall wclk rclk n a .
                 KnownNat n
              => SClock wclk               -- ^ 'Clock' to which to synchronise
                                           -- the write port of the RAM
              -> SClock rclk               -- ^ 'Clock' to which the read
                                           -- address signal, @r@, is
                                           -- synchronised
              -> Signal' wclk (Unsigned n) -- ^ Write address @w@
              -> Signal' rclk (Unsigned n) -- ^ Read address @r@
              -> Signal' wclk Bool         -- ^ Write enable
              -> Signal' wclk a            -- ^ Value to write (at address @w@)
              -> Signal' rclk a
              -- ^ Value of the @RAM@ at address @r@
asyncRamPow2' wclk rclk = asyncRam' wclk rclk (pow2SNat (SNat @ n))

{-# INLINE asyncRam' #-}
-- | Create a RAM with space for @n@ elements
--
-- * __NB__: Initial content of the RAM is 'undefined'
--
-- Additional helpful information:
--
-- * See "CLaSH.Prelude.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRam' :: Enum addr
          => SClock wclk       -- ^ 'Clock' to which to synchronise the write
                               -- port of the RAM
          -> SClock rclk       -- ^ 'Clock' to which the read address signal,
                               -- @r@, is synchronised
          -> SNat n            -- ^ Size @n@ of the RAM
          -> Signal' wclk addr -- ^ Write address @w@
          -> Signal' rclk addr -- ^ Read address @r@
          -> Signal' wclk Bool -- ^ Write enable
          -> Signal' wclk a    -- ^ Value to write (at address @w@)
          -> Signal' rclk a    -- ^ Value of the @RAM@ at address @r@
asyncRam' wclk rclk sz wr rd en din = asyncRam# wclk rclk sz (fromEnum <$> wr)
                                                (fromEnum <$> rd) en din

{-# NOINLINE asyncRam# #-}
-- | RAM primitive
asyncRam# :: SClock wclk       -- ^ 'Clock' to which to synchronise the write
                               -- port of the RAM
          -> SClock rclk       -- ^ 'Clock' to which the read address signal,
                               -- @r@, is synchronised
          -> SNat n            -- ^ Size @n@ of the RAM
          -> Signal' wclk Int  -- ^ Write address @w@
          -> Signal' rclk Int  -- ^ Read address @r@
          -> Signal' wclk Bool -- ^ Write enable
          -> Signal' wclk a    -- ^ Value to write (at address @w@)
          -> Signal' rclk a    -- ^ Value of the @RAM@ at address @r@
asyncRam# wclk rclk sz wr rd en din = unsafeSynchronizer wclk rclk dout
  where
    szI  = snatToNum sz
    rd'  = unsafeSynchronizer rclk wclk rd
    dout = runST $ do
      arr <- newListArray (0,szI-1) (replicate szI (errorX "asyncRam#: initial value undefined"))
      traverse (ramT arr) (bundle (wr,rd',en,din))

    ramT :: STArray s Int e -> (Int,Int,Bool,e) -> ST s e
    ramT ram (w,r,e,d) = do
      -- reading from address using an 'X' exception results in an 'X' result
      r' <- unsafeIOToST (catch (evaluate r >>= (return . Right))
                                (\(err :: XException) -> return (Left (throw err))))
      d' <- case r' of
              Right r2 -> readArray ram r2
              Left err -> return err
      -- writing to an address using an 'X' exception makes everything 'X'
      when e (writeArray ram w d)
      return d'
