module TestIndex where
import CLaSH.Prelude

type NrI = Index 8

topEntity = c1

c1 :: Signal (Maybe NrI) -> Signal (Maybe NrI)
c1 = fmap (fmap (+1))
