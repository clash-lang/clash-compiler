{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd,
                  2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

RAM primitives with a combinational read port.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE Trustworthy #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.RAM
  ( -- * RAM synchronized to an arbitrary clock
    asyncRam
  , asyncRamPow2
    -- * Internal
  , asyncRam#
  )
where

import Data.Maybe            (isJust)
import GHC.Stack             (HasCallStack, withFrozenCallStack)
import GHC.TypeLits          (KnownNat)
import qualified Data.Sequence as Seq

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Explicit.Signal (unbundle, KnownDomain, andEnable)
import Clash.Promoted.Nat    (SNat (..), snatToNum, pow2SNat)
import Clash.Signal.Internal (Clock (..), Signal (..), Enable, fromEnable)
import Clash.Signal.Internal.Ambiguous (clockPeriod)
import Clash.Sized.Unsigned  (Unsigned)
import Clash.XException
  (defaultSeqX, errorX, fromJustX, maybeIsX, NFDataX)

-- | Create a RAM with space for 2^@n@ elements
--
-- * __NB__: Initial content of the RAM is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
-- Additional helpful information:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRamPow2
  :: forall wdom rdom n a
   . ( KnownNat n
     , HasCallStack
     , KnownDomain wdom
     , KnownDomain rdom
     , NFDataX a
     )
  => Clock wdom
  -- ^ 'Clock' to which to synchronize the write port of the RAM
  -> Clock rdom
  -- ^ 'Clock' to which the read address signal, @r@, is synchronized
  -> Enable wdom
  -- ^ Global enable
  -> Signal rdom (Unsigned n)
  -- ^ Read address @r@
  -> Signal wdom (Maybe (Unsigned n, a))
  -- ^ (write address @w@, value to write)
  -> Signal rdom a
  -- ^ Value of the @RAM@ at address @r@
asyncRamPow2 = \wclk rclk en rd wrM -> withFrozenCallStack
  (asyncRam wclk rclk en (pow2SNat (SNat @n)) rd wrM)
{-# INLINE asyncRamPow2 #-}


-- | Create a RAM with space for @n@ elements
--
-- * __NB__: Initial content of the RAM is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
-- Additional helpful information:
--
-- * See "Clash.Explicit.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRam
  :: ( Enum addr
     , HasCallStack
     , KnownDomain wdom
     , KnownDomain rdom
     , NFDataX a
     )
  => Clock wdom
   -- ^ 'Clock' to which to synchronize the write port of the RAM
  -> Clock rdom
   -- ^ 'Clock' to which the read address signal, @r@, is synchronized to
  -> Enable wdom
  -- ^ Global enable
  -> SNat n
  -- ^ Size @n@ of the RAM
  -> Signal rdom addr
  -- ^ Read address @r@
  -> Signal wdom (Maybe (addr, a))
  -- ^ (write address @w@, value to write)
  -> Signal rdom a
   -- ^ Value of the @RAM@ at address @r@
asyncRam = \wclk rclk gen sz rd wrM ->
  let en       = isJust <$> wrM
      (wr,din) = unbundle (fromJustX <$> wrM)
  in  withFrozenCallStack
      (asyncRam# wclk rclk gen sz (fromEnum <$> rd) en (fromEnum <$> wr) din)
{-# INLINE asyncRam #-}

-- | RAM primitive
asyncRam#
  :: forall wdom rdom n a
   . ( HasCallStack
     , KnownDomain wdom
     , KnownDomain rdom
     , NFDataX a
     )
  => Clock wdom
  -- ^ 'Clock' to which to synchronize the write port of the RAM
  -> Clock rdom
  -- ^ 'Clock' to which the read address signal, @r@, is synchronized
  -> Enable wdom
  -- ^ Global enable
  -> SNat n
  -- ^ Size @n@ of the RAM
  -> Signal rdom Int
  -- ^ Read address @r@
  -> Signal wdom Bool
  -- ^ Write enable
  -> Signal wdom Int
  -- ^ Write address @w@
  -> Signal wdom a
  -- ^ Value to write (at address @w@)
  -> Signal rdom a
  -- ^ Value of the @RAM@ at address @r@
asyncRam# !_ !_ en sz rd we wr din = dout
  where
    ramI = Seq.replicate
              szI
              (withFrozenCallStack (errorX "asyncRam#: initial value undefined"))
    en0 = fromEnable (andEnable en we)
    dout = if rPeriod == wPeriod
           then goSingle ramI rd en0 wr din
           else go 0 ramI rd en0 wr din
    rPeriod = snatToNum (clockPeriod @rdom) :: Int
    wPeriod = snatToNum (clockPeriod @wdom) :: Int
    szI = snatToNum sz :: Int

    goSingle :: Seq.Seq a -> Signal rdom Int -> Signal wdom Bool
       -> Signal wdom Int -> Signal wdom a -> Signal rdom a
    goSingle !ram (r :- rs) ~(e :- es) wt@(~(w :- ws)) dt@(~(d :- ds)) =
      let ram0 = upd ram e w d
          o    = ram `safeAt` r
      in  o :- (o `defaultSeqX` wt `seq` dt `seq` goSingle ram0 rs es ws ds)

    -- Given
    --   tR = absolute time of next active edge of read clock
    --   tW = absolute time of next active edge of write clock
    -- relTime is defined as relTime = tW - tR
    --
    -- Put differently, relative time 0 points at the next active edge of the
    -- read clock, and relTime points at the next active edge of the write
    -- clock.
    go :: Int -> Seq.Seq a -> Signal rdom Int -> Signal wdom Bool
       -> Signal wdom Int -> Signal wdom a -> Signal rdom a
    go   relTime !ram rt@(~(r :- rs)) et@(~(e :- es)) wt@(~(w :- ws))
         dt@(~(d :- ds))
      | relTime < 0 = let ram0 = upd ram e w d
                      in wt `seq` dt `seq`
                         go (relTime + wPeriod) ram0 rt es ws ds
      | otherwise   = let o = ram `safeAt` r
                      in o :- (o `defaultSeqX` go (relTime - rPeriod) ram rs et wt dt)

    upd ram we0 waddr d = case maybeIsX we0 of
      Nothing -> case maybeIsX waddr of
        Nothing -> -- Put the XException from `waddr` as the value in all
                   -- locations of `ram`.
                   seq waddr d <$ ram
        Just wa -> -- Put the XException from `we` as the value at address
                   -- `waddr`.
                   safeUpdate wa (seq we0 d) ram
      Just True -> case maybeIsX waddr of
        Nothing -> -- Put the XException from `waddr` as the value in all
                   -- locations of `ram`.
                   seq waddr d <$ ram
        Just wa -> d `defaultSeqX` safeUpdate wa d ram
      _ -> ram

    safeAt :: HasCallStack => Seq.Seq a -> Int -> a
    safeAt s i =
      if (0 <= i) && (i < szI) then
        Seq.index s i
      else
        withFrozenCallStack
          (errorX ("asyncRam: read address " ++ show i ++
                   " not in range [0.." ++ show szI ++ ")"))
    {-# INLINE safeAt #-}

    safeUpdate :: HasCallStack => Int -> a -> Seq.Seq a ->  Seq.Seq a
    safeUpdate i a s =
      if (0 <= i) && (i < szI) then
        Seq.update i a s
      else
        let d = withFrozenCallStack
                  (errorX ("asyncRam: write address " ++ show i ++
                           " not in range [0.." ++ show szI ++ ")"))
        in d <$ s
    {-# INLINE safeUpdate #-}
{-# NOINLINE asyncRam# #-}
{-# ANN asyncRam# hasBlackBox #-}
