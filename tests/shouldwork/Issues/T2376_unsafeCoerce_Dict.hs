module T2376_unsafeCoerce_Dict where

import Clash.Prelude
import Data.Constraint
import Data.Proxy
import Unsafe.Coerce

data T depth = T (BitVector depth) deriving (Generic)

instance (1 <= CLog 2 depth, KnownNat depth) => NFDataX (T depth)

-- | if (2 <= n) holds, then (1 <= CLog 2 n) also holds.
oneLeCLog2n :: forall n . (2 <= n) => Proxy n -> Dict (1 <= CLog 2 n)
oneLeCLog2n Proxy = unsafeCoerce (Dict :: Dict ())

f ::
  forall dom depth.
  ( HiddenClockResetEnable dom
  , KnownNat depth
  , 2 <= depth ) =>
  Proxy depth ->
  Signal dom Bool ->
  Signal dom Bool
f Proxy =
  case oneLeCLog2n (Proxy @depth) of
    Dict -> mealy go (T 0)

 where
  go :: T depth -> Bool -> (T depth, Bool)
  go (T n) True = (T (n + 1), False)
  go (T n) False = (T (n - 1), True)
{-# NOINLINE f #-}

topEntity clk rst ena =
  withClockResetEnable clk rst ena $
    f @System @2 Proxy
{-# NOINLINE topEntity #-}
