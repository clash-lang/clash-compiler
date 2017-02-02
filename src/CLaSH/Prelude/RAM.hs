{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

RAM primitives with a combinational read port.
-}

{-# LANGUAGE BangPatterns        #-}
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

import Data.Maybe             (fromJust, isJust)
import GHC.TypeLits           (KnownNat)
import qualified Data.Vector  as V

import CLaSH.Promoted.Nat     (SNat (..), snatToNum, pow2SNat)
import CLaSH.Signal           (Signal)
import CLaSH.Signal.Bundle    (unbundle)
import CLaSH.Signal.Explicit  (SClock, systemClock, unsafeSynchronizer)
import CLaSH.Signal.Internal  (Signal' (..))
import CLaSH.Sized.Unsigned   (Unsigned)
import CLaSH.XException       (errorX)

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
         -> Signal addr -- ^ Read address @r@
         -> Signal (Maybe (addr, a))
          -- ^ (write address @w@, value to write)
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
             => Signal (Unsigned n) -- ^ Read address @r@
             -> Signal (Maybe (Unsigned n, a))
             -- ^ (write address @w@, value to write)
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
              -> Signal' rclk (Unsigned n) -- ^ Read address @r@
              -> Signal' wclk (Maybe (Unsigned n, a))
              -- ^ (write address @w@, value to write)
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
          -> Signal' rclk addr -- ^ Read address @r@
          -> Signal' wclk (Maybe (addr, a))
          -- ^ (write address @w@, value to write)
          -> Signal' rclk a    -- ^ Value of the @RAM@ at address @r@
asyncRam' wclk rclk sz rd wrM =
  let en       = isJust <$> wrM
      (wr,din) = unbundle (fromJust <$> wrM)
  in  asyncRam# wclk rclk sz (fromEnum <$> rd) en (fromEnum <$> wr) din

-- | RAM primitive
asyncRam# :: SClock wclk       -- ^ 'Clock' to which to synchronise the write
                               -- port of the RAM
          -> SClock rclk       -- ^ 'Clock' to which the read address signal,
                               -- @r@, is synchronised
          -> SNat n            -- ^ Size @n@ of the RAM
          -> Signal' rclk Int  -- ^ Read address @r@
          -> Signal' wclk Bool -- ^ Write enable
          -> Signal' wclk Int  -- ^ Write address @w@
          -> Signal' wclk a    -- ^ Value to write (at address @w@)
          -> Signal' rclk a    -- ^ Value of the @RAM@ at address @r@
asyncRam# wclk rclk sz rd en wr din = unsafeSynchronizer wclk rclk dout
  where
    rd'  = unsafeSynchronizer rclk wclk rd
    ramI = V.replicate (snatToNum sz) (errorX "asyncRam#: initial value undefined")
    dout = go ramI rd' en wr din

    go :: V.Vector a -> Signal' wclk Int -> Signal' wclk Bool
       -> Signal' wclk Int -> Signal' wclk a -> Signal' wclk a
    go !ram (r :- rs) (e :- es) (w :- ws) (d :- ds) =
      let ram' = upd ram e w d
          o    = ram V.! r
      in  o :- go ram' rs es ws ds

    upd ram True  addr d = ram V.// [(addr,d)]
    upd ram False _    _ = ram
{-# NOINLINE asyncRam# #-}
