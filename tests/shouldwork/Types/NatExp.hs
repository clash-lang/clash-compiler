-- Make sure Clash can evaluate exponentation on type-level 'Nat's
-- see https://github.com/clash-lang/clash-compiler/issues/960
module NatExp where
import Clash.Prelude

triggered2 :: forall (n :: Nat) (d :: Nat)
  .  ( KnownNat n,KnownNat d,1 <= n,1 <= d)
  => SNat n
  -> SNat d
  -> BitVector ((d*n)^2)
triggered2 _ _ = pack (repeat False :: Vec ((d*n)^2) Bool)

triggered3 :: forall (n :: Nat) (d :: Nat)
  .  ( KnownNat n,KnownNat d,1 <= n,1 <= d)
  => SNat n
  -> SNat d
  -> BitVector ((d*n)^2)
triggered3 _ _ = 0

topEntity :: (BitVector 36, BitVector 36)
topEntity = (triggered2 d2 d3, triggered3 d2 d3)
