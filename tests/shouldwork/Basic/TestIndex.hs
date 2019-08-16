module TestIndex where
import Clash.Prelude

type NrI = SatIndex 'SatError 8

topEntity = c1

c1 :: Signal System (Maybe NrI) -> Signal System (Maybe NrI)
c1 = fmap (fmap (+1))
