module T3084 where

import Clash.Prelude hiding (SNat, Mod)
import Clash.Annotations.TH (makeTopEntity)

import Data.Type.Equality
import GHC.TypeLits (SNat)

type P = 2 ^ 256 - 2 ^ 224 + 2 ^ 192 + 2 ^ 96 - 1 -- any small-ish number would work (e.g. 300)

topEntity :: "hey" ::: Unsigned (CLog 2 P) -> "ho" ::: Index P
topEntity = f

f :: forall m. (KnownNat m, 1 <= m) => Unsigned (CLog 2 m) -> Index m
f =
 case testEquality (natSing :: SNat m) (natSing :: SNat P) of
  Just Refl -> g -- g @P would work
  Nothing   -> bitCoerce

g :: forall m. (KnownNat m, 1 <= m) => Unsigned (CLog 2 m) -> Index m
g =
  case testEquality (natSing :: SNat m) (natSing :: SNat P) of
    Just Refl -> h
    Nothing   -> bitCoerce

h :: forall m . (KnownNat m, 1 <= m) => Unsigned (CLog 2 m) -> Index m
h i = 1 + bitCoerce i

makeTopEntity 'topEntity
