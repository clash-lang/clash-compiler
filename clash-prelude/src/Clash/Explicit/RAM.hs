{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd,
                  2021     , QBayLogic B.V.
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

import Clash.Explicit.Signal
  (unbundle, unsafeSynchronizer, KnownDomain, enable)
import Clash.Promoted.Nat    (SNat (..), snatToNum, pow2SNat)
import Clash.Signal.Internal (Clock (..), Signal (..), Enable, fromEnable)
import Clash.Sized.Unsigned  (Unsigned)
import Clash.XException      (errorX, maybeIsX, fromJustX)

-- | Create a RAM with space for 2^@n@ elements
--
-- * __NB__: Initial content of the RAM is 'undefined'
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
-- * __NB__: Initial content of the RAM is 'undefined'
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
  :: ( HasCallStack
     , KnownDomain wdom
     , KnownDomain rdom )
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
asyncRam# wclk rclk en sz rd we wr din =
    unsafeSynchronizer wclk rclk dout
  where
    rd'  = unsafeSynchronizer rclk wclk rd
    ramI = Seq.replicate
              (snatToNum sz)
              (withFrozenCallStack (errorX "asyncRam#: initial value undefined"))
    en' = fromEnable (enable en we)
    dout = go ramI rd' en' wr din
    szI = snatToNum sz :: Int

    go :: Seq.Seq a -> Signal wdom Int -> Signal wdom Bool
       -> Signal wdom Int -> Signal wdom a -> Signal wdom a
    go !ram (r :- rs) (e :- es) (w :- ws) (d :- ds) =
      let ram' = upd ram e (fromEnum w) d
          o    = ram `safeAt` r
      in  o :- go ram' rs es ws ds

    upd ram we' waddr d = case maybeIsX we' of
      Nothing -> case maybeIsX waddr of
        Nothing -> fmap (const (seq waddr d)) ram
        Just wa -> safeUpdate wa d ram
      Just True -> case maybeIsX waddr of
        Nothing -> fmap (const (seq waddr d)) ram
        Just wa -> safeUpdate wa d ram
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
        withFrozenCallStack
          (errorX ("asyncRam: write address " ++ show i ++
                   " not in range [0.." ++ show szI ++ ")"))
    {-# INLINE safeUpdate #-}
{-# NOINLINE asyncRam# #-}
