{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module T967b where

import Clash.Prelude

topEntity :: SystemClockResetEnable => Signal System Bool
topEntity = setName @"myRegister" (delay @System True topEntity)
