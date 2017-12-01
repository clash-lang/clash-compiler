module FromJust where

import Data.Maybe
import Clash.Prelude

{-# ANN dut (defTop { t_name = "FromJust_topEntity" }) #-}

dut :: Signal System (Maybe Int)
    -> Signal System Int
dut a = fmap fromJust a
