module T2623CaseConFVs where
import Clash.Prelude

topEntity = foo @System

foo :: forall dom. Signal dom (Vec 1 (Signed 2)) -> Signal dom Bool
foo = \input ->
    let
      scs :: Signal dom (Vec 1 Bool)
      scs = bundle $ map f $ unbundle input
    in
      fmap bar scs


bar :: (KnownNat n) => Vec (n+1) Bool -> Bool
bar = fold (&&) . map (id)

f :: Signal dom a -> Signal dom Bool
f = const $ pure $ True
