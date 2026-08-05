{-# LANGUAGE UndecidableInstances #-}
-- | Type families over newtypes used to make Clash loop forever, because the
-- casts connecting the newtype-wrapped and unwrapped representations were
-- dropped in the GHC-to-Clash translation. See
-- https://github.com/clash-lang/clash-compiler/issues/2882
module T2882 where

import Clash.Prelude

class KnownNat (UsbSize a) => UsbSerialize a where
  type UsbSize a :: Nat
  usbDeserialize :: BitVector (UsbSize a) -> a

newtype Packed a = Packed { unPacked :: a }

instance BitPack a => UsbSerialize (Packed a) where
  type UsbSize (Packed a) = BitSize a
  usbDeserialize = Packed . unpack

instance UsbSerialize Bit where
  type UsbSize Bit = UsbSize (Packed Bit)
  usbDeserialize = unPacked . usbDeserialize

topEntity ::
  Clock System -> Reset System -> Enable System ->
  Signal System Bit
topEntity clk rst en =
  withClockResetEnable clk rst en (mealy step 0 (pure ()))
 where
  step x _ = (x, usbDeserialize (x .<<+ 0))
