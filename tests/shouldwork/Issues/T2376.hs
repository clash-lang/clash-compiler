{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Casts originating from 'unsafeCoerce'd constraint evidence used to be
-- dropped in the GHC-to-Clash translation, leading to a @zipEqual@ error in
-- @caseCon@. See https://github.com/clash-lang/clash-compiler/issues/2376
module T2376 where

import Clash.Prelude
import Data.Kind (Constraint)
import Data.Proxy
import Unsafe.Coerce

data Dict (c :: Constraint) where
  Dict :: c => Dict c

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

topEntity ::
  Clock System -> Reset System -> Enable System ->
  Signal System Bool -> Signal System Bool
topEntity clk rst ena =
  withClockResetEnable clk rst ena $
    f @System @2 Proxy
{-# NOINLINE topEntity #-}
