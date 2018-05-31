{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Myrtle Software Ltd, Google Inc.
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

module Clash.Explicit.RAM
  ( -- * RAM synchronised to an arbitrary clock
    asyncRam
  , asyncRamPow2
    -- * Internal
  , asyncRam#
  )
where

import Data.Maybe            (fromJust, isJust)
import GHC.Stack             (HasCallStack, withFrozenCallStack)
import GHC.TypeLits          (KnownNat)
import qualified Data.Vector as V

import Clash.Explicit.Signal ((.&&.), unbundle, unsafeSynchronizer)
import Clash.Promoted.Nat    (SNat (..), snatToNum, pow2SNat)
import Clash.Signal.Internal (Clock (..), Signal (..), clockEnable)
import Clash.Sized.Unsigned  (Unsigned)
import Clash.XException      (errorX, maybeX)

-- | Create a RAM with space for 2^@n@ elements
--
-- * __NB__: Initial content of the RAM is 'undefined'
--
-- Additional helpful information:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRamPow2
  :: forall wdom rdom wgated rgated n a
   . (KnownNat n, HasCallStack)
  => Clock wdom wgated
  -- ^ 'Clock' to which to synchronise the write port of the RAM
  -> Clock rdom rgated
  -- ^ 'Clock' to which the read address signal, @r@, is synchronised
  -> Signal rdom (Unsigned n)
  -- ^ Read address @r@
  -> Signal wdom (Maybe (Unsigned n, a))
  -- ^ (write address @w@, value to write)
  -> Signal rdom a
  -- ^ Value of the @RAM@ at address @r@
asyncRamPow2 = \wclk rclk rd wrM -> withFrozenCallStack
  (asyncRam wclk rclk (pow2SNat (SNat @ n)) rd wrM)
{-# INLINE asyncRamPow2 #-}


-- | Create a RAM with space for @n@ elements
--
-- * __NB__: Initial content of the RAM is 'undefined'
--
-- Additional helpful information:
--
-- * See "Clash.Explicit.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRam
  :: (Enum addr, HasCallStack)
  => Clock wdom wgated
   -- ^ 'Clock' to which to synchronise the write port of the RAM
  -> Clock rdom rgated
   -- ^ 'Clock' to which the read address signal, @r@, is synchronised
  -> SNat n
  -- ^ Size @n@ of the RAM
  -> Signal rdom addr
  -- ^ Read address @r@
  -> Signal wdom (Maybe (addr, a))
  -- ^ (write address @w@, value to write)
  -> Signal rdom a
   -- ^ Value of the @RAM@ at address @r@
asyncRam = \wclk rclk sz rd wrM ->
  let en       = isJust <$> wrM
      (wr,din) = unbundle (fromJust <$> wrM)
  in  withFrozenCallStack
      (asyncRam# wclk rclk sz (fromEnum <$> rd) en (fromEnum <$> wr) din)
{-# INLINE asyncRam #-}

-- | RAM primitive
asyncRam#
  :: HasCallStack
  => Clock wdom wgated
  -- ^ 'Clock' to which to synchronise the write port of the RAM
  -> Clock rdom rgated
  -- ^ 'Clock' to which the read address signal, @r@, is synchronised
  -> SNat n            -- ^ Size @n@ of the RAM
  -> Signal rdom Int  -- ^ Read address @r@
  -> Signal wdom Bool -- ^ Write enable
  -> Signal wdom Int  -- ^ Write address @w@
  -> Signal wdom a    -- ^ Value to write (at address @w@)
  -> Signal rdom a    -- ^ Value of the @RAM@ at address @r@
asyncRam# wclk rclk sz rd en wr din =
    unsafeSynchronizer wclk rclk dout
  where
    rd'  = unsafeSynchronizer rclk wclk rd
    ramI = V.replicate
              (snatToNum sz)
              (withFrozenCallStack (errorX "asyncRam#: initial value undefined"))
    en'  = case clockEnable wclk of
             Nothing  -> en
             Just wgt -> wgt .&&. en
    dout = go ramI rd' en' wr din

    go :: V.Vector a -> Signal wdom Int -> Signal wdom Bool
       -> Signal wdom Int -> Signal wdom a -> Signal wdom a
    go !ram (r :- rs) (e :- es) (w :- ws) (d :- ds) =
      let ram' = upd ram e (fromEnum w) d
          o    = ram V.! r
      in  o :- go ram' rs es ws ds

    upd ram we waddr d = case maybeX we of
      Nothing -> case maybeX waddr of
        Nothing -> V.map (const (seq waddr d)) ram
        Just wa -> ram V.// [(wa,d)]
      Just True -> case maybeX waddr of
        Nothing -> V.map (const (seq waddr d)) ram
        Just wa -> ram V.// [(wa,d)]
      _ -> ram
{-# NOINLINE asyncRam# #-}
