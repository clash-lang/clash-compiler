module T967b where

import Clash.Prelude

topEntity = setName @"myRegister" (delay @System True topEntity)
