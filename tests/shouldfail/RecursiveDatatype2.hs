-- Clash should refuse to compile this because Ra and Rb are mutually recursive
module RecursiveDatatype2 where
import Clash.Prelude

data Ra = A1 Bool | A2 Rb
data Rb = B1 ()   | B2 Ra
data T = T Ra

topEntity :: T -> Bool
topEntity x = case x of
  T (A1 b) -> b
  T _      -> False
