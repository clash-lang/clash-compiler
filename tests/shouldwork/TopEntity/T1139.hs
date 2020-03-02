module T1139 where
import Clash.Prelude

topEntity, otherTopEntity :: Bool -> Bool
topEntity = not
{-# NOINLINE topEntity #-}

otherTopEntity = topEntity
{-# ANN otherTopEntity (defSyn "otherTopEntity") #-}
