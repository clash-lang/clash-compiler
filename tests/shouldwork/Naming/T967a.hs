module T967a where

import Clash.Prelude

topEntity x = setName @"myRegister" (delay @System True x)
