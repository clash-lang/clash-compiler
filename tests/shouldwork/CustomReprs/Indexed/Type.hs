module Type where

import Clash.Prelude

data WithVector
  = MkTA (Vec 2 Bool) (Signed 2)
  | MkTB Bool
