module EnumTypes where

import Clash.Prelude

data Valid = Valid | Invalid
data Exec  = Exec  | NOP

topEntity :: Vec 2 Valid -> Vec 2 Exec
topEntity xs = map convert xs where
  convert Valid   = Exec
  convert Invalid = NOP
